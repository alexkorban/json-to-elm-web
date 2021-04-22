port module Main exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Debounce exposing (Debounce)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Keyed
import Json
import Json.Decode
import Json.Encode
import Task


port copySignal : () -> Cmd msg


type Output
    = None
    | Error String
    | Code ( List Json.TypeString, List Json.DecoderString, List Json.EncoderString )


type alias Model =
    { debounce : Debounce String
    , elmResult : Output
    , jsonText : String
    , urlPrefix : String
    , windowSize : Size
    }


type alias Size =
    { height : Int, width : Int }


type Msg
    = DebouncerProducedMessage Debounce.Msg
    | DebouncerRequestedConversion String
    | UserPressedCopyButton
    | UserResizedWindow Int Int
    | UserTypedJson String


type alias Flags =
    { urlPrefix : String
    , windowHeight : Int
    , windowWidth : Int
    }



-- MAIN --


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 1000
    , transform = DebouncerProducedMessage
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { debounce = Debounce.init
      , elmResult = None
      , jsonText = ""
      , urlPrefix = flags.urlPrefix
      , windowSize = { height = flags.windowHeight, width = flags.windowWidth }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize UserResizedWindow


view : Model -> Html Msg
view model =
    let
        onChangeJson =
            Json.Decode.string
                |> Json.Decode.at [ "target", "editorText" ]
                |> Json.Decode.map UserTypedJson
                |> Html.Events.on "change"
    in
    layout [ width fill, height fill, baseTypeface, Font.size 18 ] <|
        column [ width fill, height fill ]
            [ navBar model
            , row [ width fill, height fill, padding 10, spacing 20 ]
                [ column [ width fill, height fill, spacing 10 ]
                    [ el [ height <| px 40 ] <| el [ centerY ] <| text "JSON sample"
                    , el [ width fill, height fill, htmlAttribute onChangeJson ] <|
                        html <|
                            Html.node "ace-editor"
                                [ Html.Attributes.attribute "mode" "ace/mode/json"
                                , Html.Attributes.attribute "wrapmode" "true"
                                , Html.Attributes.attribute "line-numbers" "false"
                                , Html.Attributes.attribute "focus" ""
                                , Html.Attributes.id "json-editor"
                                , Html.Attributes.style "height" "100%"
                                , Html.Attributes.style "border-radius" "6px"
                                ]
                                []
                    ]
                , column [ width fill, height fill, spacing 10 ]
                    [ row [ width fill, height <| px 40 ]
                        [ el [ centerY ] <| text "Elm decoders and encoders"
                        , Input.button
                            [ paddingEach { left = 20, right = 20, top = 5, bottom = 7 }
                            , alignRight
                            , centerY
                            , Border.width 1
                            , Border.rounded 6
                            , Border.color color.blue
                            , Background.color color.lightBlue
                            , Font.color color.white
                            , mouseDown
                                [ Background.color color.palerBlue
                                , Font.color color.white
                                ]
                            , mouseOver
                                [ Background.color color.palerBlue
                                , Font.color color.blue
                                ]
                            ]
                            { onPress = Just UserPressedCopyButton, label = el [ centerY ] <| text "Copy" }
                        ]
                    , el [ width fill, height fill ] <|
                        html <|
                            Html.node "ace-editor"
                                ([ Html.Attributes.attribute "wrapmode" "true"
                                 , Html.Attributes.attribute "line-numbers" "false"
                                 , Html.Attributes.id "elm-editor"
                                 , Html.Attributes.style "height" "100%"
                                 ]
                                    ++ (case model.elmResult of
                                            Code ( typeStrs, decoderStrs, encoderStrs ) ->
                                                [ Html.Attributes.attribute "mode" "ace/mode/elm"
                                                , Html.Attributes.attribute "text" <|
                                                    String.join "\n\n\n"
                                                        [ String.join "\n\n\n" typeStrs
                                                        , String.join "\n\n\n" decoderStrs
                                                        , String.join "\n\n\n" encoderStrs
                                                        ]
                                                ]

                                            Error err ->
                                                [ Html.Attributes.attribute "mode" "ace/mode/text"
                                                , Html.Attributes.attribute "text" err
                                                ]

                                            None ->
                                                [ Html.Attributes.attribute "mode" "ace/mode/elm"
                                                , Html.Attributes.attribute "text" ""
                                                ]
                                       )
                                )
                                []
                    ]
                ]
            , footer
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DebouncerProducedMessage msg_ ->
            let
                ( debounce, cmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeLast (Task.perform DebouncerRequestedConversion << Task.succeed))
                        msg_
                        model.debounce
            in
            ( { model | debounce = debounce }
            , cmd
            )

        DebouncerRequestedConversion s ->
            if String.isEmpty s then
                ( { model | elmResult = None }, Cmd.none )

            else
                case Json.convert s of
                    Err err ->
                        ( { model | elmResult = Error err }, Cmd.none )

                    Ok res ->
                        ( { model | elmResult = Code res }, Cmd.none )

        UserPressedCopyButton ->
            ( model, copySignal () )

        UserResizedWindow width height ->
            ( { model | windowSize = { height = height, width = width } }, Cmd.none )

        UserTypedJson s ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceConfig s model.debounce
            in
            ( { model
                | jsonText = s
                , debounce = debounce
              }
            , cmd
            )


attrNone =
    htmlAttribute <| Html.Attributes.class " "


baseTypeface : Element.Attribute msg
baseTypeface =
    Font.family [ Font.typeface "Open Sans", Font.typeface "Helvetica", Font.typeface "Arial", Font.serif ]


headingTypeface : Element.Attribute msg
headingTypeface =
    Font.family [ Font.typeface "Cairo", Font.typeface "Helvetica", Font.sansSerif ]


sides =
    { top = 0, bottom = 0, left = 0, right = 0 }


navBar : Model -> Element Msg
navBar model =
    row
        [ width fill
        , height <| px 60
        , spacing 20
        , paddingEach { left = 10, right = 10, top = 5, bottom = 5 }
        , Background.color color.pale
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color color.burntOrange
        , headingTypeface
        ]
        [ link [ centerY, height <| px 50 ]
            { url = "https://korban.net/posts/elm"
            , label = image [ width (px 46), height (px 50) ] { src = "https://korban.net/img/logo.png", description = "Korban.net" }
            }
        , el [ centerY, headingTypeface, Font.color <| rgb255 0x22 0x55 0x71, Font.size 20, Font.bold, Font.letterSpacing 1.5 ] <| text "json2elm"
        , if model.windowSize.width > 1100 then
            el
                [ paddingEach { left = 20, top = 2, right = 0, bottom = 0 }
                , centerY
                , Font.color color.lightCharcoal
                , Font.size 16
                ]
            <|
                text "Generate Elm code to handle JSON from a JSON sample"

          else
            none
        , link [ centerY, alignRight, Font.color color.blue, Font.underline ] { url = "https://korban.net/elm/elm-book", label = text "Practical Elm book" }
        , link [ centerY, alignRight, Font.color color.blue, Font.underline ] { url = "https://korban.net/elm/elm-ui-guide", label = text "elm-ui Guide: The CSS Escape Plan" }
        ]


footer : Element Msg
footer =
    row
        [ width fill
        , height <| px 51
        , paddingEach { top = 0, bottom = 0, left = 10, right = 10 }
        , spacing 20
        , Background.color color.veryPaleBlue
        , Border.color color.blue
        , Border.widthEach { sides | top = 1 }
        , Font.color color.blue
        , Font.size 12
        ]
        [ link [ centerY, height <| px 50 ]
            { url = "https://korban.net/elm/about"
            , label = image [ width (px 23), height (px 25) ] { src = "https://korban.net/img/logo.png", description = "Korban.net" }
            }
        , link [] { url = "https://korban.net/elm/about", label = text "A project of korban.net" }
        , link [ alignRight ] { url = "https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/", label = text "Made with mdgriffith/elm-ui" }
        ]


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


color =
    { blue = rgb255 0x34 0x65 0x78
    , burntOrange = rgb255 0xE4 0x8B 0x48
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , green = rgb255 0x64 0x77 0x33
    , grey = rgb255 0xD3 0xD7 0xCF
    , lightBlue = rgb255 0x72 0x9F 0xCF
    , lightCharcoal = rgb255 0x88 0x8A 0x85
    , lightGreen = rgb255 115 210 22
    , lighterGreen = rgb255 189 239 139
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , orange = rgba255 242 100 25 0.4
    , pale = rgb255 0xFF 0xFC 0xF6
    , paleBlue = rgb255 162 229 249
    , palerBlue = rgb255 197 232 247
    , veryPaleBlue = rgb255 242 252 255
    , white = rgb255 0xFF 0xFF 0xFF
    }
