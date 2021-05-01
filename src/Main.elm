port module Main exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Char exposing (isAlphaNum)
import Debounce exposing (Debounce)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Events exposing (onClick)
import Json
import Json.Decode
import Task


port copySignal : () -> Cmd msg


type Output
    = None
    | Error String
    | Code Json.Output


type alias Model =
    { debounce : Debounce ( TypeName, JsonString )
    , decoderStyle : Json.DecoderStyle
    , elmResult : Output
    , jsonText : JsonString
    , namingStyle : Json.NamingStyle
    , showSettings : Bool
    , topLevelTypeName : String
    , windowSize : Size
    }


type alias Size =
    { height : Int, width : Int }


type alias TypeName =
    String


type alias JsonString =
    String


type Msg
    = DebouncerProducedMessage Debounce.Msg
    | DebouncerRequestedConversion ( TypeName, JsonString )
    | UserPressedCopyButton
    | UserPressedSettingsButton
    | UserResizedWindow Int Int
    | UserSelectedDecoderStyle Json.DecoderStyle
    | UserSelectedNamingStyle Json.NamingStyle
    | UserTypedJson String
    | UserTypedTopLevelTypeName String


type alias Flags =
    { windowHeight : Int
    , windowWidth : Int
    }


type SelectorButtonPosition
    = First
    | Mid
    | Last



-- MAIN --


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.later 1000
    , transform = DebouncerProducedMessage
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { debounce = Debounce.init
      , decoderStyle = Json.PlainDecoders
      , elmResult = None
      , jsonText = ""
      , namingStyle = Json.NounNaming
      , showSettings = False
      , topLevelTypeName = "Root"
      , windowSize = { height = flags.windowHeight, width = flags.windowWidth }
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onResize UserResizedWindow


selectorButton : SelectorButtonPosition -> String -> Input.OptionState -> Element Msg
selectorButton position label state =
    let
        borders =
            case position of
                First ->
                    { left = 1, right = 1, top = 1, bottom = 1 }

                Mid ->
                    { left = 0, right = 1, top = 1, bottom = 1 }

                Last ->
                    { left = 0, right = 1, top = 1, bottom = 1 }

        corners =
            case position of
                First ->
                    { topLeft = 6, bottomLeft = 6, topRight = 0, bottomRight = 0 }

                Mid ->
                    { topLeft = 0, bottomLeft = 0, topRight = 0, bottomRight = 0 }

                Last ->
                    { topLeft = 0, bottomLeft = 0, topRight = 6, bottomRight = 6 }
    in
    el
        [ paddingEach { left = 10, right = 10, top = 5, bottom = 5 }
        , Border.roundEach corners
        , Border.widthEach borders
        , Border.color color.blue
        , Background.color <|
            if state == Input.Selected then
                color.lightBlue

            else
                color.white
        , Font.color <|
            if state == Input.Selected then
                color.white

            else
                color.blue
        ]
    <|
        el [ centerX, centerY ] <|
            text label


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
            , row [ width fill, height fill, padding 10, spacing 10 ]
                [ column [ width fill, height fill, spacing 10 ]
                    [ el [ alignBottom, height <| px 30 ] <| el [ alignBottom ] <| text "JSON sample"
                    , el [ width fill, height fill, htmlAttribute onChangeJson ] <|
                        html <|
                            Html.node "ace-editor"
                                [ Html.Attributes.attribute "mode" "ace/mode/json"
                                , Html.Attributes.attribute "wrapmode" "true"
                                , Html.Attributes.attribute "line-numbers" "false"
                                , Html.Attributes.attribute "placeholder" "Enter a JSON sample"
                                , Html.Attributes.attribute "focus" ""
                                , Html.Attributes.id "json-editor"
                                , Html.Attributes.style "height" "100%"
                                , Html.Attributes.style "border-radius" "6px"
                                ]
                                []
                    ]
                , column [ width fill, height fill ]
                    [ row [ width fill, height <| px 40, spacing 5 ]
                        [ el [ height <| px 30 ] <| el [ centerY ] <| text "Elm decoders and encoders"
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
                        , el
                            [ paddingEach { left = 20, right = 20, top = if_ model.showSettings 8 5, bottom = if_ model.showSettings 12 7 }
                            , Background.color color.pale
                            , Font.color color.blue
                            , Border.width 1
                            , if_ model.showSettings (Border.roundEach { topLeft = 6, topRight = 6, bottomLeft = 0, bottomRight = 0 }) (Border.rounded 6)
                            , Border.widthEach { top = 1, left = 1, right = 1, bottom = if_ model.showSettings 0 1 }
                            , Border.color color.lightGrey
                            , if_ model.showSettings alignBottom centerY
                            , if_ model.showSettings (below <| el [ width fill, height <| px 2, Background.color color.pale ] none) attrNone
                            , pointer
                            , Events.onClick UserPressedSettingsButton
                            , htmlAttribute <| Html.Attributes.style "-webkit-user-select" "none"
                            , htmlAttribute <| Html.Attributes.style "-moz-user-select" "none"
                            , htmlAttribute <| Html.Attributes.style "-ms-user-select" "none"
                            ]
                          <|
                            text "Settings"
                        ]
                    , if model.showSettings then
                        let
                            nameIsValid =
                                isValidTypeName model.topLevelTypeName
                        in
                        el
                            [ width fill
                            , padding 10
                            , Background.color color.pale
                            , Border.color color.lightGrey
                            , Border.width 1
                            , Border.roundEach { topLeft = 4, topRight = 0, bottomLeft = 4, bottomRight = 4 }
                            , Font.size 16
                            ]
                        <|
                            column [ spacing 10 ]
                                [ Input.text [ padding 5, if_ nameIsValid attrNone (Border.color color.burntOrange) ]
                                    { onChange = UserTypedTopLevelTypeName
                                    , text = model.topLevelTypeName
                                    , placeholder = Just <| Input.placeholder [] <| text "\"Root\" if unspecified"
                                    , label = Input.labelLeft [ if_ nameIsValid attrNone (Font.color color.burntOrange), padding 5 ] <| text "Top level type name"
                                    }
                                , row [ spacing 30 ]
                                    [ Input.radioRow []
                                        { onChange = UserSelectedNamingStyle
                                        , selected = Just model.namingStyle
                                        , label =
                                            Input.labelLeft [ padding 5 ] <| text "Naming style"
                                        , options =
                                            [ Input.optionWith Json.NounNaming <| selectorButton First "Noun"
                                            , Input.optionWith Json.VerbNaming <| selectorButton Last "Verb"
                                            ]
                                        }
                                    , Input.radioRow []
                                        { onChange = UserSelectedDecoderStyle
                                        , selected = Just model.decoderStyle
                                        , label =
                                            Input.labelLeft [ padding 5 ] <| text "Decoder style"
                                        , options =
                                            [ Input.optionWith Json.PlainDecoders <| selectorButton First "Plain"
                                            , Input.optionWith Json.ApplicativeDecoders <| selectorButton Mid "Applicative"
                                            , Input.optionWith Json.PipelineDecoders <| selectorButton Last "Pipeline"
                                            ]
                                        }
                                    ]
                                ]

                      else
                        none
                    , if_ model.showSettings (el [ height <| px 5 ] none) none
                    , el [ width fill, height fill ] <|
                        html <|
                            Html.node "ace-editor"
                                ([ Html.Attributes.attribute "wrapmode" "true"
                                 , Html.Attributes.attribute "line-numbers" "false"
                                 , Html.Attributes.id "elm-editor"
                                 , Html.Attributes.style "height" "100%"
                                 ]
                                    ++ (case model.elmResult of
                                            Code { imports, types, decoders, encoders } ->
                                                [ Html.Attributes.attribute "mode" "ace/mode/elm"
                                                , Html.Attributes.attribute "text" <|
                                                    if List.isEmpty types then
                                                        String.join "\n\n\n"
                                                            [ String.join "\n" imports
                                                            , String.join "\n\n\n" decoders
                                                            , String.join "\n\n\n" encoders
                                                            ]

                                                    else
                                                        String.join "\n\n\n"
                                                            [ String.join "\n" imports
                                                            , String.join "\n\n\n" types
                                                            , String.join "\n\n\n" decoders
                                                            , String.join "\n\n\n" encoders
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

        DebouncerRequestedConversion ( topLevelTypeName, jsonText ) ->
            if String.isEmpty jsonText then
                ( { model | elmResult = None }, Cmd.none )

            else
                case Json.convert { rootTypeName = topLevelTypeName, decoderStyle = model.decoderStyle, namingStyle = model.namingStyle } jsonText of
                    Err err ->
                        ( { model | elmResult = Error err }, Cmd.none )

                    Ok res ->
                        ( { model | elmResult = Code res }, Cmd.none )

        UserPressedCopyButton ->
            ( model, copySignal () )

        UserPressedSettingsButton ->
            ( { model | showSettings = not model.showSettings }, Cmd.none )

        UserResizedWindow width height ->
            ( { model | windowSize = { height = height, width = width } }, Cmd.none )

        UserSelectedDecoderStyle decoderStyle ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceConfig ( model.topLevelTypeName, model.jsonText ) model.debounce
            in
            ( { model | debounce = debounce, decoderStyle = decoderStyle }, cmd )

        UserSelectedNamingStyle namingStyle ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceConfig ( model.topLevelTypeName, model.jsonText ) model.debounce
            in
            ( { model | debounce = debounce, namingStyle = namingStyle }, cmd )

        UserTypedJson s ->
            let
                ( debounce, cmd ) =
                    Debounce.push debounceConfig ( model.topLevelTypeName, s ) model.debounce
            in
            ( { model
                | jsonText = s
                , debounce = debounce
              }
            , cmd
            )

        UserTypedTopLevelTypeName name ->
            if isValidTypeName name then
                let
                    ( debounce, cmd ) =
                        Debounce.push debounceConfig ( name, model.jsonText ) model.debounce
                in
                ( { model
                    | topLevelTypeName = name
                    , debounce = debounce
                  }
                , cmd
                )

            else
                ( { model | topLevelTypeName = name }, Cmd.none )


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
        , height <| px 36
        , paddingEach { top = 0, bottom = 0, left = 10, right = 10 }
        , spacing 10
        , Background.color color.veryPaleBlue
        , Border.color color.blue
        , Border.widthEach { sides | top = 1 }
        , Font.color color.blue
        , Font.size 12
        ]
        [ link [ centerY, height <| px 25 ]
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


if_ : Bool -> a -> a -> a
if_ predicate lhs rhs =
    if predicate then
        lhs

    else
        rhs


isValidTypeName : TypeName -> Bool
isValidTypeName name =
    String.all (\c -> Char.isAlphaNum c || c == '_') name && (String.all Char.isAlpha <| String.left 1 name)


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
