module Json exposing (DecoderString, DecoderStyle(..), EncoderString, GeneratorOptions, JsonString, NamingStyle(..), TypeString, convert)

import Array exposing (Array)
import Char exposing (isDigit)
import Cons exposing (Cons)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import List.Extra
import Set exposing (Set)
import String.Extra


type JsonValue
    = JString String
    | JFloat Float
    | JInt Int
    | JBool Bool
    | JList (List Node)
    | JObj (List Node)
    | JNull


type alias JsonString =
    String


type alias DecoderString =
    String


type alias EncoderString =
    String


type alias TypeString =
    String


type alias Path =
    Cons String


type alias Node =
    { value : JsonValue
    , path : Path
    }


type DecoderStyle
    = PlainDecoders
    | PipelineDecoders


type NamingStyle
    = VerbNaming
    | NounNaming


type alias GeneratorOptions =
    { rootTypeName : String, decoderStyle : DecoderStyle, namingStyle : NamingStyle }


convert :
    GeneratorOptions
    -> JsonString
    -> Result String ( List TypeString, List DecoderString, List EncoderString )
convert ({ rootTypeName } as options) jsonStr =
    case parse jsonStr of
        Err err ->
            Err <| Decode.errorToString err

        Ok tree ->
            tree
                |> annotate
                    (Cons.singleton
                        (if String.isEmpty rootTypeName then
                            "Root"

                         else
                            rootTypeName
                        )
                    )
                --|> Debug.log "tree"
                |> (\t -> ( typesAndAliases t, plainDecoders options t, encoders options.namingStyle t ))
                |> Ok


jsonDecoder : Decoder Node
jsonDecoder =
    let
        makeNode v =
            -- unfortunately have to create a fake path value as there's no way to combine
            -- recursive JsonValue with two node types (without a path and with a path)
            { value = v, path = Cons.singleton "" }

        withAttrNames keyValuePairs =
            keyValuePairs
                |> List.map (\( attrName, node ) -> { node | path = Cons.singleton attrName })
    in
    Decode.oneOf
        [ Decode.map (makeNode << JString) Decode.string
        , Decode.map (makeNode << JInt) Decode.int
        , Decode.map (makeNode << JFloat) Decode.float
        , Decode.map (makeNode << JBool) Decode.bool
        , Decode.map (makeNode << JList) <| Decode.list <| Decode.lazy (\_ -> jsonDecoder)
        , Decode.map (makeNode << JObj << withAttrNames) <| Decode.keyValuePairs <| Decode.lazy (\_ -> jsonDecoder)
        , Decode.null (makeNode JNull)
        ]


parse : String -> Result Decode.Error Node
parse json =
    Decode.decodeString jsonDecoder json


annotate : Path -> Node -> Node
annotate pathSoFar node =
    let
        indexNoun =
            Array.fromList [ "Object", "Item", "Entity", "Thing", "Instance", "Constituent", "Specimen", "Gadget", "Widget", "Gizmo", "Part", "Chunk", "Piece", "Thingy", "Thingamajig", "Whatsit", "Doodad" ]

        strFromIndex index =
            Maybe.withDefault ("Alias" ++ String.fromInt index) <| Array.get index indexNoun

        annotateList index listNode =
            annotate (Cons.appendList pathSoFar [ strFromIndex index ]) listNode

        annotateObj objNode =
            annotate
                (Cons.appendList pathSoFar <|
                    if String.isEmpty <| Cons.head objNode.path then
                        []

                    else
                        [ Cons.head objNode.path ]
                )
                objNode
    in
    case node.value of
        JList children ->
            { node
                | path = pathSoFar
                , value = JList <| List.indexedMap annotateList children
            }

        JObj children ->
            { node
                | path = pathSoFar
                , value = JObj <| List.map annotateObj children
            }

        _ ->
            { node | path = pathSoFar }



-- GENERATION OF TYPES AND TYPE ALIASES --


typesAndAliases : Node -> List String
typesAndAliases node =
    case node.value of
        JList nodes ->
            listTypesAndAliases node.path nodes

        JObj nodes ->
            objTypeAlias node.path nodes
                :: (nodes
                        |> List.filter producesNestedTypes
                        |> List.map typesAndAliases
                        |> List.concat
                   )

        _ ->
            []


listTypesAndAliases : Path -> List Node -> List String
listTypesAndAliases path childNodes =
    let
        elmTypes =
            List.map elmType childNodes
                |> Set.fromList
    in
    if Set.size elmTypes > 1 then
        -- heterogeneous array
        customType path (Set.toList elmTypes)
            :: (childNodes
                    |> List.filter producesNestedTypes
                    |> List.map typesAndAliases
                    |> List.concat
               )

    else
        case List.head childNodes of
            Nothing ->
                []

            Just childNode ->
                typesAndAliases childNode


typeAliasName : Path -> String
typeAliasName path =
    String.Extra.classify <|
        if Cons.length path > 1 then
            String.join " " <| Tuple.second <| Cons.uncons path

        else
            Cons.head path


elmType : Node -> String
elmType { path, value } =
    case value of
        JBool _ ->
            "Bool"

        JFloat _ ->
            "Float"

        JInt _ ->
            "Int"

        JString _ ->
            "String"

        JObj _ ->
            typeAliasName path

        JList children ->
            "List " ++ (paren <| listTypeName path children)

        JNull ->
            "()"


objTypeAlias : Path -> List Node -> String
objTypeAlias path nodes =
    nodes
        |> List.map
            (\node ->
                (adorn <| Cons.head <| Cons.reverse node.path) ++ " : " ++ elmType node
            )
        |> List.sort
        |> String.join "\n    , "
        |> (\fieldStr ->
                ("type alias " ++ typeAliasName path ++ " =\n")
                    ++ "    "
                    ++ (if String.isEmpty fieldStr then
                            "{}"

                        else
                            "{ " ++ fieldStr ++ "\n    }"
                       )
           )


isObj : JsonValue -> Bool
isObj val =
    case val of
        JObj _ ->
            True

        _ ->
            False


isList : JsonValue -> Bool
isList val =
    case val of
        JList _ ->
            True

        _ ->
            False


isHeterogeneous : List Node -> Bool
isHeterogeneous nodes =
    let
        elmTypes =
            List.map elmType nodes
                |> Set.fromList
    in
    Set.size elmTypes > 1


producesNestedTypes : Node -> Bool
producesNestedTypes { value } =
    case value of
        JObj _ ->
            True

        JList childNodes ->
            List.any (\item -> isObj item.value || isList item.value) childNodes
                || isHeterogeneous childNodes

        _ ->
            False


listTypeName : Path -> List Node -> String
listTypeName path nodes =
    let
        elmTypes =
            List.map elmType nodes
                |> Set.fromList
    in
    case Set.size elmTypes of
        0 ->
            "()"

        1 ->
            Maybe.withDefault "ERROR" <| List.head <| Set.toList elmTypes

        _ ->
            typeAliasName path


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


customType : Path -> List String -> String
customType path elmTypes =
    let
        name =
            typeAliasName path
    in
    "type "
        ++ name
        ++ "\n    = "
        ++ (elmTypes
                |> (\lst ->
                        -- the List () type has to be pushed to the end to match the decoder
                        -- where it *has to be* at the end to allow other list decoders to be
                        -- tried first
                        case List.Extra.elemIndex "List ()" lst of
                            Nothing ->
                                lst

                            Just i ->
                                List.append (List.Extra.removeAt i lst) [ "List ()" ]
                   )
                |> List.indexedMap (\i t -> name ++ String.fromInt i ++ " " ++ paren t)
                |> String.join "\n    | "
           )



-- GENERATION OF PLAIN DECODERS --


plainDecoders : GeneratorOptions -> Node -> List String
plainDecoders options node =
    case node.value of
        JList nodes ->
            listDecoders options node nodes

        JObj nodes ->
            objDecoders options.namingStyle options.decoderStyle node.path nodes
                :: (nodes
                        |> List.filter producesNestedTypes
                        |> List.map (plainDecoders options)
                        |> List.concat
                   )

        _ ->
            let
                funcName =
                    decoderFuncName options.namingStyle <| typeAliasName node.path
            in
            [ (funcName ++ " : Json.Decode.Decoder " ++ elmType node ++ "\n")
                ++ (funcName ++ " = \n")
                ++ "    "
                ++ decoderName options.namingStyle node
            ]


listDecoders : GeneratorOptions -> Node -> List Node -> List String
listDecoders options node childNodes =
    let
        names =
            Set.fromList <| List.map (decoderName options.namingStyle) childNodes

        typeName =
            typeAliasName node.path

        firstIs s tuple =
            Tuple.first tuple == s

        funcName =
            decoderFuncName options.namingStyle typeName

        memberFuncName =
            decoderFuncName options.namingStyle (typeName ++ "Member")

        listDecoder =
            (funcName ++ " : Json.Decode.Decoder " ++ (paren <| elmType node) ++ "\n")
                ++ (funcName ++ " = \n")
                ++ (String.repeat 4 " " ++ "Json.Decode.list " ++ memberFuncName)

        mainDecoder =
            listDecoder
                ++ "\n\n\n"
                ++ (memberFuncName ++ " : Json.Decode.Decoder " ++ (paren <| listTypeName node.path childNodes) ++ "\n")
                ++ (memberFuncName ++ " = \n")
                ++ "    "
                ++ (case Set.size names of
                        0 ->
                            "Json.Decode.succeed ()"

                        1 ->
                            case List.head childNodes of
                                -- cannot happen when set size is 1
                                Nothing ->
                                    "ERROR"

                                Just childNode ->
                                    decoderName options.namingStyle childNode

                        _ ->
                            -- heterogeneous array
                            "Json.Decode.oneOf\n"
                                ++ String.repeat 8 " "
                                ++ "[ "
                                ++ (childNodes
                                        |> List.map (\n -> ( elmType n, n ))
                                        |> List.Extra.uniqueBy Tuple.first
                                        |> List.sortBy Tuple.first
                                        |> (\lst ->
                                                -- the decoder for an empty array has to be pushed to the end
                                                -- to allow other list decoders to be tried first
                                                case List.Extra.findIndex (firstIs "List ()") lst of
                                                    Nothing ->
                                                        lst

                                                    Just i ->
                                                        case List.Extra.getAt i lst of
                                                            Just tuple ->
                                                                List.append (List.Extra.removeAt i lst) [ tuple ]

                                                            Nothing ->
                                                                -- cannot happen but we cannot tell the type system that
                                                                lst
                                           )
                                        |> List.map (Tuple.second >> decoderName options.namingStyle)
                                        |> List.indexedMap
                                            (\i name ->
                                                "Json.Decode.map " ++ typeName ++ String.fromInt i ++ " <| " ++ name
                                            )
                                        |> String.join ("\n" ++ String.repeat 8 " " ++ ", ")
                                   )
                                ++ "\n"
                                ++ String.repeat 8 " "
                                ++ "]"
                   )
    in
    mainDecoder
        :: (childNodes
                |> List.filter producesNestedTypes
                |> List.map (plainDecoders options)
                |> List.concat
           )


objDecoders : NamingStyle -> DecoderStyle -> Path -> List Node -> String
objDecoders namingStyle decoderStyle path childNodes =
    let
        typeName =
            typeAliasName path

        sortedChildNodes =
            List.sortBy (adorn << Cons.head << Cons.reverse << .path) childNodes

        funcName =
            decoderFuncName namingStyle typeName
    in
    (funcName ++ " : Json.Decode.Decoder " ++ typeName ++ "\n")
        ++ (funcName ++ " = \n")
        ++ (case ( List.length sortedChildNodes, decoderStyle ) of
                ( 0, _ ) ->
                    "    Json.Decode.succeed " ++ typeName

                ( 1, PlainDecoders ) ->
                    "    Json.Decode.map " ++ typeName ++ "\n" ++ objFieldDecoders 8 namingStyle sortedChildNodes

                ( fieldCount, PlainDecoders ) ->
                    if fieldCount > 8 then
                        stagedObjDecoders namingStyle typeName sortedChildNodes

                    else
                        "    Json.Decode.map"
                            ++ (String.fromInt <| List.length sortedChildNodes)
                            ++ " "
                            ++ typeName
                            ++ "\n"
                            ++ objFieldDecoders 8 namingStyle sortedChildNodes

                ( _, PipelineDecoders ) ->
                    "    Json.Decode.succeed "
                        ++ typeName
                        ++ "\n"
                        ++ pipelineObjFieldDecoders 8 namingStyle sortedChildNodes
           )


objFieldDecoders : Int -> NamingStyle -> List Node -> String
objFieldDecoders indent namingStyle nodes =
    nodes
        |> List.map
            (\node ->
                String.repeat indent " "
                    ++ "(Json.Decode.field \""
                    ++ (Cons.head <| Cons.reverse node.path)
                    ++ "\" "
                    ++ (withApplyArrow <| decoderName namingStyle node)
                    ++ ")"
            )
        |> String.join "\n"


stagedObjDecoders : NamingStyle -> String -> List Node -> String
stagedObjDecoders namingStyle typeName nodes =
    let
        initFieldSet =
            List.take 8 nodes

        fieldSets =
            nodes
                |> List.drop 8
                |> List.Extra.greedyGroupsOf 7
    in
    "    let\n"
        ++ (String.repeat 8 " " ++ "fieldSet0 = \n")
        ++ (String.repeat 12 " " ++ "Json.Decode.map8 " ++ typeName ++ "\n")
        ++ objFieldDecoders 16 namingStyle initFieldSet
        ++ "\n"
        ++ (fieldSets
                |> List.indexedMap
                    (\index fieldSet ->
                        if List.length fieldSet == 7 && index < List.length fieldSets - 1 then
                            ("\n" ++ String.repeat 8 " " ++ "fieldSet" ++ String.fromInt (index + 1) ++ " =\n")
                                ++ (String.repeat 12 " " ++ "Json.Decode.map8 (<|)\n")
                                ++ (String.repeat 16 " " ++ "fieldSet" ++ String.fromInt index ++ "\n")
                                ++ objFieldDecoders 16 namingStyle fieldSet

                        else
                            "    in\n"
                                ++ (String.repeat 4 " " ++ "Json.Decode.map" ++ String.fromInt (1 + List.length fieldSet) ++ " (<|)\n")
                                ++ (String.repeat 8 " " ++ "fieldSet" ++ String.fromInt (List.length fieldSets - 1) ++ "\n")
                                ++ objFieldDecoders 8 namingStyle fieldSet
                    )
                |> String.join "\n"
           )


listDecoderName : NamingStyle -> Path -> List Node -> String
listDecoderName namingStyle path nodes =
    let
        decoderNames =
            List.map (decoderName namingStyle) nodes
                |> Set.fromList
    in
    if Set.size decoderNames == 1 then
        "Json.Decode.list " ++ (paren <| Maybe.withDefault "ERROR" <| List.head <| Set.toList decoderNames)

    else
        decoderFuncName namingStyle <| typeAliasName path


decoderFuncName : NamingStyle -> String -> String
decoderFuncName namingStyle typeName =
    if namingStyle == NounNaming then
        String.Extra.decapitalize <| typeName ++ "Decoder"

    else
        "decode" ++ typeName


decoderName : NamingStyle -> Node -> String
decoderName namingStyle { path, value } =
    case value of
        JInt _ ->
            "Json.Decode.int"

        JFloat _ ->
            "Json.Decode.float"

        JString _ ->
            "Json.Decode.string"

        JBool _ ->
            "Json.Decode.bool"

        JList [] ->
            -- an empty list cannot be decoded as a list because the type of values is unknown
            "Json.Decode.list <| Json.Decode.succeed ()"

        JList nodes ->
            listDecoderName namingStyle path nodes

        JObj _ ->
            decoderFuncName namingStyle <| typeAliasName path

        JNull ->
            "Json.Decode.null ()"



-- GENERATION OF PIPELINE DECODERS


pipelineObjFieldDecoders : Int -> NamingStyle -> List Node -> String
pipelineObjFieldDecoders indent namingStyle nodes =
    nodes
        |> List.map
            (\node ->
                String.repeat indent " "
                    ++ "|> Json.Decode.Pipeline.required \""
                    ++ (Cons.head <| Cons.reverse node.path)
                    ++ "\" "
                    ++ (paren <| decoderName namingStyle node)
            )
        |> String.join "\n"



-- GENERATION OF ENCODERS --


encoders : NamingStyle -> Node -> List String
encoders namingStyle node =
    let
        namePrefix =
            if namingStyle == NounNaming then
                "encoded"

            else
                "encode"

        typeName =
            typeAliasName node.path
    in
    case node.value of
        JList nodes ->
            listEncoders namingStyle node nodes

        JObj nodes ->
            objEncoders namingStyle node.path nodes
                :: (nodes
                        |> List.filter producesNestedTypes
                        |> List.map (encoders namingStyle)
                        |> List.concat
                   )

        _ ->
            [ (namePrefix ++ typeName ++ " : " ++ elmType node ++ " -> Json.Encode.Value\n")
                ++ (namePrefix ++ typeName ++ " " ++ String.Extra.decapitalize typeName ++ " =\n")
                ++ "    "
                ++ encoderName namingStyle (String.Extra.decapitalize typeName) node
            ]


objEncoders : NamingStyle -> Path -> List Node -> String
objEncoders namingStyle path childNodes =
    let
        typeName =
            typeAliasName path

        fieldEncoders =
            childNodes
                |> List.map
                    (\node ->
                        "( \""
                            ++ (Cons.head <| Cons.reverse node.path)
                            ++ "\", "
                            ++ encoderName namingStyle (String.Extra.decapitalize typeName ++ "." ++ (adorn <| Cons.head <| Cons.reverse node.path)) node
                            ++ " )"
                    )
                |> List.sort
                |> String.join ("\n" ++ String.repeat 8 " " ++ ", ")
    in
    (encoderNamePrefix namingStyle ++ typeName ++ " : " ++ typeName ++ " -> Json.Encode.Value\n")
        ++ (encoderNamePrefix namingStyle ++ typeName ++ " " ++ String.Extra.decapitalize typeName ++ " = \n")
        ++ "    Json.Encode.object\n"
        ++ String.repeat 8 " "
        ++ (if String.isEmpty fieldEncoders then
                "[]"

            else
                "[ " ++ fieldEncoders ++ ("\n" ++ String.repeat 8 " " ++ "]")
           )


listEncoders : NamingStyle -> Node -> List Node -> List String
listEncoders namingStyle node childNodes =
    let
        names =
            Set.fromList <| List.map (encoderName namingStyle "") childNodes

        typeName =
            typeAliasName node.path

        firstIs s tuple =
            Tuple.first tuple == s

        listEncoder =
            (encoderNamePrefix namingStyle ++ typeName ++ " : ")
                ++ ("List " ++ (paren <| listTypeName node.path childNodes) ++ " -> Json.Encode.Value\n")
                ++ (encoderNamePrefix namingStyle ++ typeName ++ " =\n")
                ++ (String.repeat 4 " " ++ "Json.Encode.list " ++ encoderNamePrefix namingStyle ++ typeName ++ "Member")

        mainEncoder =
            listEncoder
                ++ "\n\n\n"
                ++ (encoderNamePrefix namingStyle ++ typeName ++ "Member : ")
                ++ (listTypeName node.path childNodes ++ " -> Json.Encode.Value\n")
                ++ (encoderNamePrefix namingStyle ++ typeName ++ "Member " ++ String.Extra.decapitalize typeName ++ " =\n")
                ++ String.repeat 4 " "
                ++ (case Set.size names of
                        0 ->
                            "Json.Encode.null"

                        1 ->
                            case List.head childNodes of
                                -- cannot happen when set size is 1
                                Nothing ->
                                    "ERROR"

                                Just childNode ->
                                    encoderName namingStyle (String.Extra.decapitalize typeName) childNode

                        _ ->
                            -- heterogeneous array
                            ("case " ++ String.Extra.decapitalize typeName ++ " of\n")
                                ++ String.repeat 8 " "
                                ++ (childNodes
                                        |> List.map (\n -> ( elmType n, n ))
                                        |> List.Extra.uniqueBy Tuple.first
                                        |> List.sortBy Tuple.first
                                        |> (\lst ->
                                                -- the decoder for an empty array has to be pushed to the end
                                                -- to allow other list decoders to be tried first
                                                case List.Extra.findIndex (firstIs "List ()") lst of
                                                    Nothing ->
                                                        lst

                                                    Just i ->
                                                        case List.Extra.getAt i lst of
                                                            Just tuple ->
                                                                List.append (List.Extra.removeAt i lst) [ tuple ]

                                                            Nothing ->
                                                                -- cannot happen but we cannot tell the type system that
                                                                lst
                                           )
                                        |> List.map (Tuple.second >> encoderName namingStyle "value")
                                        |> List.indexedMap
                                            (\i name ->
                                                (typeName ++ String.fromInt i ++ " value ->\n")
                                                    ++ (String.repeat 12 " " ++ name)
                                            )
                                        |> String.join ("\n\n" ++ String.repeat 8 " ")
                                   )
                   )
    in
    mainEncoder
        :: (childNodes
                |> List.filter producesNestedTypes
                |> List.map (encoders namingStyle)
                |> List.concat
           )


listEncoderName : NamingStyle -> Path -> List Node -> String
listEncoderName namingStyle path nodes =
    let
        encoderNames =
            List.map (encoderName namingStyle "") nodes
                |> Set.fromList
    in
    if Set.size encoderNames == 1 then
        "Json.Encode.list " ++ (paren <| Maybe.withDefault "ERROR" <| List.head <| Set.toList encoderNames)

    else
        encoderNamePrefix namingStyle ++ typeAliasName path


encoderName : NamingStyle -> String -> Node -> String
encoderName namingStyle valueName { path, value } =
    String.trimRight <|
        case value of
            JInt _ ->
                "Json.Encode.int " ++ valueName

            JFloat _ ->
                "Json.Encode.float " ++ valueName

            JString _ ->
                "Json.Encode.string " ++ valueName

            JBool _ ->
                "Json.Encode.bool " ++ valueName

            JList [] ->
                -- an empty list is a special case because there is no member type
                "Json.Encode.list (\\_ -> Json.Encode.null) []"

            JList nodes ->
                listEncoderName namingStyle path nodes ++ " " ++ valueName

            JObj _ ->
                encoderNamePrefix namingStyle ++ typeAliasName path ++ " " ++ valueName

            JNull ->
                "Json.Encode.null"


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


adorn : String -> String
adorn fieldName =
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
                        if String.isEmpty name || startsWithDigit name then
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
