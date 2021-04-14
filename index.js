const R = require("ramda")
const Util = require("util")

const Elm = require("./cli").Elm

const main = Elm.CommandLine.init()

// Get data from the command line
const args = process.argv.slice(2)
const input = args[0]
console.log("\n   Input: ", input)

// Send data to the worker
main.ports.input.send(["id", input])

// Get data from the worker
main.ports.output.subscribe(function(data) {
    console.log("   Output: " + JSON.stringify(data) + "\n")
})