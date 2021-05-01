port module CommandLine exposing (main)

import Json exposing (DecoderStyle)
import Platform exposing (Program)


type alias Id =
    String


type alias InputType =
    { id : String
    , json : Json.JsonString
    , namingStyle : String
    , decoderStyle : String
    }


type alias OutputType =
    { id : String
    , json : String
    , namingStyle : String
    , decoderStyle : String
    , error : String
    , output : Json.Output
    }


port input : (InputType -> msg) -> Sub msg


port output : OutputType -> Cmd msg


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    ()


type Msg
    = Input InputType


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )


resultAsRecord :
    InputType
    -> Result String Json.Output
    -> OutputType
resultAsRecord { id, json, namingStyle, decoderStyle } res =
    case res of
        Err err ->
            { id = id
            , json = json
            , namingStyle = namingStyle
            , decoderStyle = decoderStyle
            , error = err
            , output = { imports = [], types = [], decoders = [], encoders = [] }
            }

        Ok result ->
            { id = id, json = json, namingStyle = namingStyle, decoderStyle = decoderStyle, error = "", output = result }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input i ->
            let
                namingStyle =
                    case i.namingStyle of
                        "noun" ->
                            Json.NounNaming

                        _ ->
                            Json.VerbNaming

                decoderStyle =
                    case i.decoderStyle of
                        "plain" ->
                            Json.PlainDecoders

                        "applicative" ->
                            Json.ApplicativeDecoders

                        _ ->
                            Json.PipelineDecoders
            in
            ( model
            , output
                (resultAsRecord i <|
                    Json.convert { rootTypeName = "Sample", namingStyle = namingStyle, decoderStyle = decoderStyle } i.json
                )
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    input Input
