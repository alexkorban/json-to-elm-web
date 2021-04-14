module Main exposing (main)

import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Json



-- MAIN --


jsonSamples =
    [ """
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
            "cards": []
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
"""
    , """
123
"""
    , """
"str"
"""
    , """
[1, [2, [3]]]
"""
    , """
    []
"""
    , """
    {}
"""
    , """
[{"red": 255, "green": 255, "blue": 255}]
"""
    , """
{"first": null, "second": [1, 2, "null", 4]}
"""
    , """
{"first": [1, [2], 3, ["4"], []]}
"""
    , """
[1, [2], 3, ["4"], null]    
"""
    ]


resultAsStr res =
    case res of
        Ok ( t, d, e ) ->
            t ++ d ++ "\n\n" ++ e

        Err s ->
            s


main =
    let
        convert jsonSample =
            case Json.convert jsonSample of
                Err s ->
                    column [ width fill ] [ text s ]

                Ok ( t, d, e ) ->
                    [ t, d, e ]
                        |> List.map
                            (\strs ->
                                column [ width fill, spacing 10 ] <|
                                    List.map (html << Html.pre [] << List.singleton << Html.text) strs
                            )
                        |> column [ width fill, spacing 30, Font.size 12 ]

        sampleView jsonSample =
            row [ width fill, Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 } ]
                [ el [ width fill ] <| text jsonSample
                , convert jsonSample
                ]
    in
    layout [] <|
        column [ spacing 10, width fill ] <|
            List.map sampleView jsonSamples
