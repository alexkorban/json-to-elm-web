port module CommandLine exposing (main)

import Json
import Platform exposing (Program)


type alias Id =
    String


type alias InputType =
    ( Id, Json.JsonString )


type alias OutputType =
    { id : String
    , json : String
    , error : String
    , types : List Json.TypeString
    , decoders : List Json.DecoderString
    , encoders : List Json.EncoderString
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
    Id
    -> Json.JsonString
    -> Result String ( List Json.TypeString, List Json.DecoderString, List Json.EncoderString )
    -> OutputType
resultAsRecord id jsonStr res =
    case res of
        Err err ->
            { id = id, json = jsonStr, error = err, types = [], decoders = [], encoders = [] }

        Ok ( types, decoders, encoders ) ->
            { id = id, json = jsonStr, error = "", types = types, decoders = decoders, encoders = encoders }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input ( id, inputStr ) ->
            ( model, output (resultAsRecord id inputStr <| Json.convert inputStr) )


subscriptions : Model -> Sub Msg
subscriptions _ =
    input Input
