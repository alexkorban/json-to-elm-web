module Json exposing (DecoderStyle(..), GeneratorOptions, JsonString, NamingStyle(..), Output, convert)

import Array exposing (Array)
import Char exposing (isDigit)
import Cons exposing (Cons)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import List.Extra exposing (unique)
import Set exposing (Set)
import String.Extra


type JsonValue
    = JString
    | JFloat
    | JInt
    | JBool
    | JList (List JsonValue)
    | JObj (List ( String, JsonValue ))
    | JNull


type alias JsonString =
    String


type alias Path =
    Cons String


type DecoderStyle
    = ApplicativeDecoders
    | PipelineDecoders
    | PlainDecoders


type NamingStyle
    = VerbNaming
    | NounNaming


type alias Output =
    { imports : List String, types : List String, decoders : List String, encoders : List String }


type alias GeneratorOptions =
    { rootTypeName : String, decoderStyle : DecoderStyle, namingStyle : NamingStyle }


asStr : JsonValue -> String
asStr value =
    case value of
        JBool ->
            "Bool"

        JInt ->
            "Int"

        JFloat ->
            "Float"

        JString ->
            "String"

        JNull ->
            "()"

        JList items ->
            "List [" ++ (String.join ", " <| List.map asStr items) ++ "]"

        JObj items ->
            "{" ++ (String.join ", " <| List.map (\( label, item ) -> label ++ ": " ++ asStr item) items) ++ "}"


convert : GeneratorOptions -> JsonString -> Result String Output
convert options jsonStr =
    case Decode.decodeString jsonDecoder jsonStr of
        Err err ->
            Err <| Decode.errorToString err

        Ok tree ->
            let
                rootTypeName =
                    if String.isEmpty options.rootTypeName then
                        "Root"

                    else
                        options.rootTypeName

                rootPath =
                    Cons.singleton rootTypeName
            in
            Ok
                { imports = imports options.decoderStyle
                , types = typesAndAliases rootPath tree
                , decoders = decoders options rootPath tree
                , encoders = encoders options.namingStyle rootPath tree
                }


jsonDecoder : Decoder JsonValue
jsonDecoder =
    Decode.oneOf
        [ Decode.map (\_ -> JString) Decode.string
        , Decode.map (\_ -> JInt) Decode.int
        , Decode.map (\_ -> JFloat) Decode.float
        , Decode.map (\_ -> JBool) Decode.bool
        , Decode.map JList <| Decode.list <| Decode.lazy (\_ -> jsonDecoder)
        , Decode.map (JObj << List.sortBy (adorn 0 << Tuple.first)) <| Decode.keyValuePairs <| Decode.lazy (\_ -> jsonDecoder)
        , Decode.null JNull
        ]


indexNouns : Array String
indexNouns =
    Array.fromList [ "Object", "Member", "Entity", "Thing", "Instance", "Constituent", "Specimen", "Gadget", "Widget", "Gizmo", "Part", "Chunk", "Piece", "Thingy", "Thingamajig", "Whatsit", "Doodad" ]


strFromIndex : Int -> String
strFromIndex index =
    Maybe.withDefault ("Alias" ++ String.fromInt index) <| Array.get index indexNouns



-- GENERATION OF IMPORTS --


imports : DecoderStyle -> List String
imports decoderStyle =
    let
        importStrs =
            [ "import Json.Decode", "import Json.Encode" ]
                ++ (case decoderStyle of
                        ApplicativeDecoders ->
                            [ "import Json.Decode.Extra" ]

                        PipelineDecoders ->
                            [ "import Json.Decode.Pipeline" ]

                        PlainDecoders ->
                            []
                   )

        commentStrs =
            [ "-- Required packages:"
            , "-- * elm/json"
            ]
                ++ (case decoderStyle of
                        ApplicativeDecoders ->
                            [ "-- * elm-community/json-extra" ]

                        PipelineDecoders ->
                            [ "-- * NoRedInk/elm-json-decode-pipeline" ]

                        PlainDecoders ->
                            []
                   )
    in
    importStrs ++ [ "\n" ] ++ commentStrs



-- GENERATION OF TYPES AND TYPE ALIASES --


typesAndAliases : Path -> JsonValue -> List String
typesAndAliases path node =
    case node of
        JList nodes ->
            listTypesAndAliases path nodes

        JObj nodeTuples ->
            objTypeAliases path nodeTuples

        _ ->
            []


{-| A value is non-trivial if it requires type or alias definitions (and corresponding decoders & encoders)
-}
isNonTrivial node =
    let
        areHeterogeneous nodes =
            (List.length <| List.Extra.uniqueBy asStr nodes) > 1
    in
    case node of
        JList nodes ->
            List.any isNonTrivial nodes || areHeterogeneous nodes

        JObj _ ->
            True

        _ ->
            False


listTypesAndAliases : Path -> List JsonValue -> List String
listTypesAndAliases path nodes =
    let
        uniqueItems =
            List.Extra.uniqueBy asStr nodes
                |> List.sortBy decoderSortOrder

        isHeterogeneous =
            List.length uniqueItems > 1
    in
    if isHeterogeneous then
        customType path uniqueItems
            :: (uniqueItems
                    |> List.indexedMap
                        (\i node ->
                            if isNonTrivial node then
                                typesAndAliases (Cons.appendList path [ strFromIndex i ]) node

                            else
                                []
                        )
                    |> List.concat
               )

    else
        case List.head uniqueItems of
            Nothing ->
                []

            Just childNode ->
                typesAndAliases (Cons.appendList path [ strFromIndex 0 ]) childNode


typeAliasName : Path -> String
typeAliasName path =
    String.Extra.classify <|
        if Cons.length path > 1 then
            String.join " " <| Cons.toList path

        else
            Cons.head path


elmType : Path -> JsonValue -> String
elmType path value =
    case value of
        JBool ->
            "Bool"

        JFloat ->
            "Float"

        JInt ->
            "Int"

        JString ->
            "String"

        JObj _ ->
            typeAliasName path

        JList nodes ->
            let
                uniqueItems =
                    List.Extra.uniqueBy asStr nodes
            in
            "List " ++ (paren <| listItemTypeName path uniqueItems)

        JNull ->
            "()"


objTypeAliases : Path -> List ( String, JsonValue ) -> List String
objTypeAliases path nodeTuples =
    let
        fieldStr =
            nodeTuples
                |> List.indexedMap
                    (\i node ->
                        (adorn i <| Tuple.first node)
                            ++ " : "
                            ++ elmType (Cons.appendList path [ Tuple.first node ]) (Tuple.second node)
                    )
                |> String.join "\n    , "

        mainAlias =
            ("type alias " ++ typeAliasName path ++ " =\n")
                ++ "    "
                ++ (if String.isEmpty fieldStr then
                        "{}"

                    else
                        "{ " ++ fieldStr ++ "\n    }"
                   )
    in
    mainAlias
        :: (nodeTuples
                |> List.filter (Tuple.second >> isNonTrivial)
                |> List.map (\( label, n ) -> typesAndAliases (Cons.appendList path [ label ]) n)
                |> List.concat
           )


{-| This function expects a list of _unique items_ in a list (rather than all of them)
-}
listItemTypeName : Path -> List JsonValue -> String
listItemTypeName path uniqueItems =
    let
        isHeterogeneous =
            List.length uniqueItems > 1
    in
    if isHeterogeneous then
        typeAliasName path

    else
        case List.head uniqueItems of
            Nothing ->
                "()"

            Just node ->
                elmType (Cons.appendList path [ strFromIndex 0 ]) node


paren : String -> String
paren t =
    if String.contains " " t then
        "(" ++ t ++ ")"

    else
        t


withApplyArrow : String -> String
withApplyArrow s =
    if String.contains " " s then
        "<| " ++ s

    else
        s


{-| An empty list has to be tried last so that other list decoders have a chance
to succeed before it
-}
decoderSortOrder : JsonValue -> Int
decoderSortOrder node =
    case node of
        JList [] ->
            1

        _ ->
            0


{-| This function expects a list of unique types
-}
customType : Path -> List JsonValue -> String
customType path nodes =
    let
        name =
            typeAliasName path
    in
    "type "
        ++ name
        ++ "\n    = "
        ++ (nodes
                |> List.indexedMap
                    (\i node ->
                        name
                            ++ String.fromInt i
                            ++ " "
                            ++ (paren <| elmType (Cons.appendList path [ strFromIndex i ]) node)
                    )
                |> String.join "\n    | "
           )



-- GENERATION OF PLAIN DECODERS --


decoders : GeneratorOptions -> Path -> JsonValue -> List String
decoders options path node =
    case node of
        JList nodes ->
            listDecoders options path node nodes

        JObj nodeTuples ->
            objDecoders options path nodeTuples

        _ ->
            let
                funcName =
                    decoderFuncName options.namingStyle <| typeAliasName path
            in
            [ (funcName ++ " : Json.Decode.Decoder " ++ elmType path node ++ "\n")
                ++ (funcName ++ " = \n")
                ++ "    "
                ++ decoderName options.namingStyle path node
            ]


listDecoders : GeneratorOptions -> Path -> JsonValue -> List JsonValue -> List String
listDecoders options path node nodes =
    let
        uniqueItems =
            List.Extra.uniqueBy asStr nodes
                |> List.sortBy decoderSortOrder

        isHeterogeneous =
            List.length uniqueItems > 1

        typeName =
            typeAliasName path

        funcName =
            decoderFuncName options.namingStyle typeName

        itemDecoder =
            if isHeterogeneous then
                "Json.Decode.list " ++ decoderFuncName options.namingStyle (typeName ++ "Item")

            else
                case List.head uniqueItems of
                    Nothing ->
                        "Json.Decode.list <| Json.Decode.succeed ()"

                    Just childNode ->
                        decoderName options.namingStyle (Cons.appendList path [ strFromIndex 0 ]) childNode

        itemFuncName =
            decoderFuncName options.namingStyle (typeName ++ "Item")

        listDecoder =
            (funcName ++ " : Json.Decode.Decoder " ++ (paren <| elmType path node) ++ "\n")
                ++ (funcName ++ " = \n")
                ++ (String.repeat 4 " " ++ listDecoderName options.namingStyle path uniqueItems)

        mainDecoders =
            (if Cons.length path == 1 then
                -- Usually we don't need to generate a separate decoder for the list itself,
                -- instead expressing things in terms of the item decoder. However,
                -- if the top level value is an array, we do need to generate a decoder for it,
                -- so we add it here
                [ listDecoder ]

             else
                []
            )
                ++ (if isHeterogeneous then
                        [ (itemFuncName ++ " : Json.Decode.Decoder " ++ (paren <| listItemTypeName path uniqueItems) ++ "\n")
                            ++ (itemFuncName ++ " = \n")
                            ++ "    "
                            ++ "Json.Decode.oneOf\n"
                            ++ String.repeat 8 " "
                            ++ "[ "
                            ++ (uniqueItems
                                    |> List.indexedMap
                                        (\i n ->
                                            "Json.Decode.map "
                                                ++ typeName
                                                ++ String.fromInt i
                                                ++ " <| "
                                                ++ decoderName options.namingStyle (Cons.appendList path [ strFromIndex i ]) n
                                        )
                                    |> String.join ("\n" ++ String.repeat 8 " " ++ ", ")
                               )
                            ++ "\n"
                            ++ String.repeat 8 " "
                            ++ "]"
                        ]

                    else
                        []
                   )
    in
    mainDecoders
        ++ (uniqueItems
                |> List.indexedMap
                    (\i n ->
                        if isNonTrivial n then
                            decoders options (Cons.appendList path [ strFromIndex i ]) n

                        else
                            []
                    )
                |> List.concat
           )


objDecoders : GeneratorOptions -> Path -> List ( String, JsonValue ) -> List String
objDecoders options path nodeTuples =
    let
        typeName =
            typeAliasName path

        funcName =
            decoderFuncName options.namingStyle typeName

        mainDecoder =
            (funcName ++ " : Json.Decode.Decoder " ++ typeName ++ "\n")
                ++ (funcName ++ " = \n")
                ++ (case ( List.length nodeTuples, options.decoderStyle ) of
                        ( 0, _ ) ->
                            "    Json.Decode.succeed " ++ typeName

                        ( 1, PlainDecoders ) ->
                            "    Json.Decode.map " ++ typeName ++ "\n" ++ objFieldDecoders 8 options.namingStyle path nodeTuples

                        ( fieldCount, PlainDecoders ) ->
                            if fieldCount > 8 then
                                stagedObjDecoders options.namingStyle typeName path nodeTuples

                            else
                                "    Json.Decode.map"
                                    ++ (String.fromInt <| List.length nodeTuples)
                                    ++ " "
                                    ++ typeName
                                    ++ "\n"
                                    ++ objFieldDecoders 8 options.namingStyle path nodeTuples

                        ( _, PipelineDecoders ) ->
                            "    Json.Decode.succeed "
                                ++ typeName
                                ++ "\n"
                                ++ pipelineObjFieldDecoders 8 options.namingStyle path nodeTuples

                        ( _, ApplicativeDecoders ) ->
                            "    Json.Decode.succeed "
                                ++ typeName
                                ++ "\n"
                                ++ applicativeObjFieldDecoders 8 options.namingStyle path nodeTuples
                   )
    in
    mainDecoder
        :: (nodeTuples
                |> List.filter (Tuple.second >> isNonTrivial)
                |> List.map (\( label, n ) -> decoders options (Cons.appendList path [ label ]) n)
                |> List.concat
           )


objFieldDecoders : Int -> NamingStyle -> Path -> List ( String, JsonValue ) -> String
objFieldDecoders indent namingStyle path nodeTuples =
    nodeTuples
        |> List.map
            (\( label, value ) ->
                String.repeat indent " "
                    ++ "(Json.Decode.field \""
                    ++ label
                    ++ "\" "
                    ++ (withApplyArrow <| decoderName namingStyle (Cons.appendList path [ label ]) value)
                    ++ ")"
            )
        |> String.join "\n"


stagedObjDecoders : NamingStyle -> String -> Path -> List ( String, JsonValue ) -> String
stagedObjDecoders namingStyle typeName path nodeTuples =
    let
        initFieldSet =
            List.take 8 nodeTuples

        fieldSets =
            nodeTuples
                |> List.drop 8
                |> List.Extra.greedyGroupsOf 7
    in
    "    let\n"
        ++ (String.repeat 8 " " ++ "fieldSet0 = \n")
        ++ (String.repeat 12 " " ++ "Json.Decode.map8 " ++ typeName ++ "\n")
        ++ objFieldDecoders 16 namingStyle path initFieldSet
        ++ "\n"
        ++ (fieldSets
                |> List.indexedMap
                    (\index fieldSet ->
                        if List.length fieldSet == 7 && index < List.length fieldSets - 1 then
                            ("\n" ++ String.repeat 8 " " ++ "fieldSet" ++ String.fromInt (index + 1) ++ " =\n")
                                ++ (String.repeat 12 " " ++ "Json.Decode.map8 (<|)\n")
                                ++ (String.repeat 16 " " ++ "fieldSet" ++ String.fromInt index ++ "\n")
                                ++ objFieldDecoders 16 namingStyle path fieldSet

                        else
                            "    in\n"
                                ++ (String.repeat 4 " " ++ "Json.Decode.map" ++ String.fromInt (1 + List.length fieldSet) ++ " (<|)\n")
                                ++ (String.repeat 8 " " ++ "fieldSet" ++ String.fromInt (List.length fieldSets - 1) ++ "\n")
                                ++ objFieldDecoders 8 namingStyle path fieldSet
                    )
                |> String.join "\n"
           )


listDecoderName : NamingStyle -> Path -> List JsonValue -> String
listDecoderName namingStyle path nodes =
    let
        uniqueItems =
            List.Extra.uniqueBy asStr nodes

        isHeterogeneous =
            List.length uniqueItems > 1

        itemDecoderFuncName =
            if namingStyle == NounNaming then
                String.Extra.decapitalize <| typeAliasName path ++ "ItemDecoder"

            else
                "decode" ++ typeAliasName path ++ "Item"
    in
    "Json.Decode.list "
        ++ (if isHeterogeneous then
                itemDecoderFuncName

            else
                case List.head uniqueItems of
                    Nothing ->
                        withApplyArrow "Json.Decode.succeed ()"

                    Just node ->
                        paren <| decoderName namingStyle (Cons.appendList path [ strFromIndex 0 ]) node
           )


decoderFuncName : NamingStyle -> String -> String
decoderFuncName namingStyle typeName =
    if namingStyle == NounNaming then
        String.Extra.decapitalize <| typeName ++ "Decoder"

    else
        "decode" ++ typeName


decoderName : NamingStyle -> Path -> JsonValue -> String
decoderName namingStyle path value =
    case value of
        JInt ->
            "Json.Decode.int"

        JFloat ->
            "Json.Decode.float"

        JString ->
            "Json.Decode.string"

        JBool ->
            "Json.Decode.bool"

        JList nodes ->
            listDecoderName namingStyle path nodes

        JObj nodeTuples ->
            decoderFuncName namingStyle <| typeAliasName path

        JNull ->
            "Json.Decode.null ()"



-- GENERATION OF PIPELINE DECODERS


pipelineObjFieldDecoders : Int -> NamingStyle -> Path -> List ( String, JsonValue ) -> String
pipelineObjFieldDecoders indent namingStyle path nodeTuples =
    nodeTuples
        |> List.map
            (\( label, node ) ->
                String.repeat indent " "
                    ++ "|> Json.Decode.Pipeline.required \""
                    ++ label
                    ++ "\" "
                    ++ (paren <| decoderName namingStyle (Cons.appendList path [ label ]) node)
            )
        |> String.join "\n"



-- GENERATION OF APPLICATIVE DECODERS


applicativeObjFieldDecoders : Int -> NamingStyle -> Path -> List ( String, JsonValue ) -> String
applicativeObjFieldDecoders indent namingStyle path nodeTuples =
    nodeTuples
        |> List.map
            (\( label, node ) ->
                String.repeat indent " "
                    ++ "|> Json.Decode.Extra.andMap ("
                    ++ "Json.Decode.field \""
                    ++ label
                    ++ "\" "
                    ++ (withApplyArrow <| decoderName namingStyle (Cons.appendList path [ label ]) node)
                    ++ ")"
            )
        |> String.join "\n"



-- GENERATION OF ENCODERS --


encoders : NamingStyle -> Path -> JsonValue -> List String
encoders namingStyle path node =
    let
        namePrefix =
            if namingStyle == NounNaming then
                "encoded"

            else
                "encode"

        typeName =
            typeAliasName path
    in
    case node of
        JList nodes ->
            listEncoders namingStyle path node nodes

        JObj nodes ->
            objEncoders namingStyle path nodes

        _ ->
            [ (namePrefix ++ typeName ++ " : " ++ elmType path node ++ " -> Json.Encode.Value\n")
                ++ (namePrefix ++ typeName ++ " " ++ String.Extra.decapitalize typeName ++ " =\n")
                ++ "    "
                ++ encoderName namingStyle (String.Extra.decapitalize typeName) path node
            ]


objEncoders : NamingStyle -> Path -> List ( String, JsonValue ) -> List String
objEncoders namingStyle path nodeTuples =
    let
        typeName =
            typeAliasName path

        fieldEncoders =
            nodeTuples
                |> List.indexedMap
                    (\i ( label, node ) ->
                        "( \""
                            ++ label
                            ++ "\", "
                            ++ encoderName namingStyle (String.Extra.decapitalize typeName ++ "." ++ adorn i label) (Cons.appendList path [ label ]) node
                            ++ " )"
                    )
                |> String.join ("\n" ++ String.repeat 8 " " ++ ", ")

        mainEncoder =
            (encoderNamePrefix namingStyle ++ typeName ++ " : " ++ typeName ++ " -> Json.Encode.Value\n")
                ++ (encoderNamePrefix namingStyle ++ typeName ++ " " ++ String.Extra.decapitalize typeName ++ " = \n")
                ++ "    Json.Encode.object\n"
                ++ String.repeat 8 " "
                ++ (if String.isEmpty fieldEncoders then
                        "[]"

                    else
                        "[ " ++ fieldEncoders ++ ("\n" ++ String.repeat 8 " " ++ "]")
                   )
    in
    mainEncoder
        :: (nodeTuples
                |> List.filter (Tuple.second >> isNonTrivial)
                |> List.map (\( label, n ) -> encoders namingStyle (Cons.appendList path [ label ]) n)
                |> List.concat
           )


listEncoders : NamingStyle -> Path -> JsonValue -> List JsonValue -> List String
listEncoders namingStyle path node childNodes =
    let
        uniqueItems =
            List.Extra.uniqueBy asStr childNodes
                |> List.sortBy decoderSortOrder

        isHeterogeneous =
            List.length uniqueItems > 1

        typeName =
            typeAliasName path

        listEncoder =
            (encoderNamePrefix namingStyle ++ typeName ++ " : ")
                ++ ("List " ++ (paren <| listItemTypeName path uniqueItems) ++ " -> Json.Encode.Value\n")
                ++ (encoderNamePrefix namingStyle ++ typeName ++ " " ++ String.Extra.decapitalize typeName ++ " =\n")
                ++ (String.repeat 4 " " ++ listEncoderName namingStyle path uniqueItems ++ " " ++ String.Extra.decapitalize typeName)

        mainEncoders =
            (if Cons.length path == 1 then
                -- Usually we don't need to generate a separate encoder function for the list itself,
                -- instead expressing things in terms of the item encoder function. However,
                -- if the top level value is an array, we do need to generate an encoder function for it,
                -- so we add it here
                [ listEncoder ]

             else
                []
            )
                ++ (if isHeterogeneous then
                        [ (encoderNamePrefix namingStyle ++ typeName ++ "Item : ")
                            ++ (listItemTypeName path uniqueItems ++ " -> Json.Encode.Value\n")
                            ++ (encoderNamePrefix namingStyle ++ typeName ++ "Item " ++ String.Extra.decapitalize typeName ++ " =\n")
                            ++ String.repeat 4 " "
                            ++ ("case " ++ String.Extra.decapitalize typeName ++ " of\n")
                            ++ String.repeat 8 " "
                            ++ (uniqueItems
                                    |> List.indexedMap
                                        (\i n ->
                                            (typeName ++ String.fromInt i ++ " value ->\n")
                                                ++ (String.repeat 12 " " ++ encoderName namingStyle "value" (Cons.appendList path [ strFromIndex i ]) n)
                                        )
                                    |> String.join ("\n\n" ++ String.repeat 8 " ")
                               )
                        ]

                    else
                        []
                   )
    in
    mainEncoders
        ++ (uniqueItems
                |> List.indexedMap
                    (\i n ->
                        if isNonTrivial n then
                            encoders namingStyle (Cons.appendList path [ strFromIndex i ]) n

                        else
                            []
                    )
                |> List.concat
           )


listEncoderName : NamingStyle -> Path -> List JsonValue -> String
listEncoderName namingStyle path nodes =
    let
        uniqueItems =
            List.Extra.uniqueBy asStr nodes

        isHeterogeneous =
            List.length uniqueItems > 1
    in
    "Json.Encode.list "
        ++ (if isHeterogeneous then
                encoderNamePrefix namingStyle ++ typeAliasName path ++ "Item"

            else
                case List.head uniqueItems of
                    Nothing ->
                        "(\\_ -> Json.Encode.null)"

                    Just node ->
                        paren <| encoderName namingStyle "" (Cons.appendList path [ strFromIndex 0 ]) node
           )


encoderName : NamingStyle -> String -> Path -> JsonValue -> String
encoderName namingStyle valueName path value =
    String.trimRight <|
        case value of
            JInt ->
                "Json.Encode.int " ++ valueName

            JFloat ->
                "Json.Encode.float " ++ valueName

            JString ->
                "Json.Encode.string " ++ valueName

            JBool ->
                "Json.Encode.bool " ++ valueName

            JNull ->
                "Json.Encode.null"

            JList nodes ->
                listEncoderName namingStyle path nodes ++ " " ++ valueName

            JObj _ ->
                encoderNamePrefix namingStyle ++ typeAliasName path ++ " " ++ valueName


encoderNamePrefix : NamingStyle -> String
encoderNamePrefix namingStyle =
    if namingStyle == NounNaming then
        "encoded"

    else
        "encode"



-- HELPERS --


keywords =
    Set.fromList
        [ "if"
        , "then"
        , "else"
        , "case"
        , "of"
        , "let"
        , "in"
        , "type"
        , "module"
        , "where"
        , "import"
        , "exposing"
        , "as"
        , "port"
        ]


adorn : Int -> String -> String
adorn fieldIndex fieldName =
    let
        startsWithDigit s =
            s
                |> String.left 1
                |> String.any isDigit

        isAllowed c =
            Char.isAlphaNum c || c == '-' || c == '_'
    in
    case String.toInt fieldName of
        Just _ ->
            "field" ++ fieldName

        Nothing ->
            fieldName
                |> String.filter (not << (==) ' ')
                |> String.toList
                |> List.map
                    (\c ->
                        if not <| isAllowed c then
                            "U" ++ (String.fromInt <| Char.toCode c)

                        else
                            String.fromChar c
                    )
                |> String.concat
                |> String.Extra.classify
                |> String.Extra.decapitalize
                |> (\name ->
                        if String.isEmpty name then
                            "field" ++ (String.fromChar <| Char.fromCode <| fieldIndex + 65)

                        else if startsWithDigit name then
                            "field" ++ name

                        else
                            name
                   )
                |> (\name ->
                        if Set.member name keywords then
                            name ++ "_"

                        else
                            name
                   )
