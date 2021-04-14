const Chalk = require("chalk")
const Fs = require("fs-extra")
const Glob = require("glob")
const Path = require("path")
const R = require("ramda")
const Util = require("util")
const { spawn } = require("cross-spawn")

const jsonSamples = [
`
{
    "account": {
        "id": 1, 
        "user" : {
            "name": "abc",
            "address": {
                "num": 1, 
                "street": "abc", 
                "postcode": 5024
            },
            "cards": [],
            "isActive": true
        }, 
        "prefs": [
            1, 
            [[[2, 3, 4]]], 
            5,
            "a", 
            {"colors": [
                    "red", 
                    {"h": 100, "s": 100, "l": 100}, 
                    [{"r": {"a": 255}   , "g": 255, "b": 255}]
                ]
            }
        ]
    }
}
`
    , `
123
`
    , `
"str"
`
    , `
[1, [2, [3]]]
`
    , `
    []
`
    , `
    {}
`
    , `
[{"red": 255, "green": 255, "blue": 255}]
`
    , `
{"first": null, "second": [1, 2, "null", 4]}
`
    , `
{"first": [1, [2], 3, ["4"], []]}
`
    , `
[1, [2], 3, ["4"], null]    
`
, `
false
`
]

const compileElm = (sourceFileNames, outputName) => {
    const args = ["make", sourceFileNames, "--optimize", "--output", `generated/${outputName}.js`]

    console.log(`  $ elm ${R.flatten(args).join(" ")}`)

    const res = spawn.sync("elm", R.flatten(args), { stdio: 'inherit' })
    if (res.status == 1) {  // This indicates a compiler error
        throw new Error("")
    }
    else if (!R.isNil(res.error) && res.error.errno == "ENOENT") {
        throw new Error(`Couldn't find the Elm executable (${res.error.path})`)
    }
    else
        ; // Compiled successfully
}


compileElm("src/CommandLine.elm", "cli")

const Generator = require("./generated/cli").Elm

const generator = Generator.CommandLine.init()


const convert = () => {
    return new Promise((resolve, reject) => {
        let results = []
        // Get data from the worker
        generator.ports.output.subscribe((data) => {
            results.push(data)
            if (R.length(results) == R.length(jsonSamples))
                resolve(results)
            else 
                ; // Keep waiting for more data to fall out of the port
        })

        // Send data to the worker
        R.addIndex(R.forEach)((json, i) => generator.ports.input.send([`${i}`, json]), jsonSamples)
    })
}

const writeTest = (elm) => {
    const test = `
port module Test${elm.id} exposing (main)

import Json.Decode
import Json.Encode
import Platform exposing (Program)

-- JSON SAMPLE:
${elm.json.split("\n").join("\n--")}


port output${elm.id} : String -> Cmd msg


type alias Flags = String


${elm.types.join("\n\n\n")}


${elm.decoders.join("\n\n\n")}


${elm.encoders.join("\n\n\n")}


init : Flags -> ( (), Cmd msg )
init json =
    ( (), output${elm.id} <| trip json )


main : Program Flags () msg
main =
    Platform.worker
        { init = init
        , update = \\msg model -> ( model, Cmd.none )
        , subscriptions = \\_ -> Sub.none
        }


trip : String -> String 
trip json =
    case Json.Decode.decodeString decodeRoot json of 
        Err err -> 
            Json.Decode.errorToString err 

        Ok value -> 
            Json.Encode.encode 4 <| encodeRoot value     
    `

    Fs.writeFileSync(Path.join(__dirname, "test", `Test${elm.id}.elm`), test)
}



convert()
.then((results) => {
    console.log("Writing Elm test files...")
    return R.forEach(writeTest, results)
})
.then((results) => {
    console.log("Compiling Elm test files...")
    compileElm(Glob.sync("test/*.elm"), "tests")

    const TestElm = require("./generated/tests.js").Elm

    R.addIndex(R.map)((jsonSample, i) => {
        TestElm[`Test${i}`].init({flags: jsonSample})
            .ports[`output${i}`]
            .subscribe((result) => {
                if (R.equals(JSON.parse(jsonSample), JSON.parse(result))) {
                    console.log(Chalk.green(`✅ Test${i}`))    
                }
                else {
                    console.log(Chalk.red(`❌ Test${i}`))    
                    console.log("Expected: \n\n" + Chalk.green(jsonSample))
                    console.log("\n\nActual: \n\n" + Chalk.red(result))
                }
            })                
    }, jsonSamples)
    
})
.catch((err) => console.log(err))

