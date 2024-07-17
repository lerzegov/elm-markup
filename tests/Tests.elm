module Tests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mrk
import Mrk.Edit
import Mrk.Internal.Description
import Mrk.Internal.Error as Error
import Mrk.Internal.Outcome
import Mrk.New
import Test exposing (..)


text =
    Mrk.text Tuple.pair


inlines =
    Mrk.document
        [ Mrk.textWith
            { view =
                \style content ->
                    [ ( style, content ) ]
            , replacements =
                [ Mrk.replacement "..." "…"
                , Mrk.replacement "<>" "\u{00A0}"
                , Mrk.replacement "---" "—"
                , Mrk.replacement "--" "–"
                , Mrk.replacement "//" "/"
                , Mrk.replacement "'" "’"
                , Mrk.balanced
                    { start = ( "\"", "“" )
                    , end = ( "\"", "”" )
                    }
                ]
            , inlines =
                [ Mrk.annotation "highlight" (\id txt -> txt)
                , Mrk.verbatim "verb" (\id str -> [ Tuple.pair emptyStyles str ])
                , Mrk.verbatim "withColor"
                    (\id str color ->
                        [ ( emptyStyles, str ++ color )
                        ]
                    )
                    |> Mrk.field "color" redOrBlue
                ]
            }
        ]


redOrBlue =
    Mrk.string
        |> Mrk.verify
            (\str ->
                if str == "red" || str == "blue" then
                    Ok str

                else
                    Err
                        { title = "Bad Color"
                        , message =
                            [ "A color must be red or blue."
                            ]
                        }
            )


inlineMultipleVerbatim =
    Mrk.document
        [ Mrk.textWith
            { view =
                \styling str ->
                    [ str ]
            , replacements =
                [ Mrk.replacement "..." "…"
                , Mrk.replacement "<>" "\u{00A0}"
                , Mrk.replacement "---" "—"
                , Mrk.replacement "--" "–"
                , Mrk.replacement "//" "/"
                , Mrk.replacement "'" "’"
                , Mrk.balanced
                    { start = ( "\"", "“" )
                    , end = ( "\"", "”" )
                    }
                ]
            , inlines =
                [ Mrk.annotation "highlight" (\id txts -> [ "high" ])
                , Mrk.verbatim "verb"
                    (\id str ->
                        [ str ]
                    )
                , Mrk.verbatim "verb2"
                    (\id str one two ->
                        [ str, one, two ]
                    )
                    |> Mrk.field "one" Mrk.string
                    |> Mrk.field "two" Mrk.string
                ]
            }
        ]


inlineOrder =
    Mrk.document
        [ Mrk.textWith
            { view = \_ _ -> Nothing
            , replacements =
                [ Mrk.replacement "..." "…"
                , Mrk.replacement "<>" "\u{00A0}"
                , Mrk.replacement "---" "—"
                , Mrk.replacement "--" "–"
                , Mrk.replacement "//" "/"
                , Mrk.replacement "'" "’"
                , Mrk.balanced
                    { start = ( "\"", "“" )
                    , end = ( "\"", "”" )
                    }
                ]
            , inlines =
                [ Mrk.annotation "order"
                    (\id txt one two ->
                        Just ( one, two )
                    )
                    |> Mrk.field "one" Mrk.string
                    |> Mrk.field "two" Mrk.string
                ]
            }
        ]


withMetaData =
    Mrk.documentWith
        { id = \_ -> "none"
        , metadata =
            Mrk.record "Meta"
                (\one two -> { one = one, two = two })
                |> Mrk.field "one" Mrk.string
                |> Mrk.field "two" Mrk.string
        , blocks =
            [ text
            ]
        }


topLevelText =
    Mrk.document
        [ text ]


textDoc =
    Mrk.document
        [ Mrk.block "Test"
            identity
            text
        ]


recordDoc =
    Mrk.document
        [ Mrk.record "Test"
            (\one two three -> { one = one, two = two, three = three })
            |> Mrk.field "one" Mrk.string
            |> Mrk.field "two" Mrk.string
            |> Mrk.field "three" Mrk.string
            |> Mrk.toBlock
        ]


recordManyTextDoc =
    Mrk.document
        [ Mrk.record "Test"
            (\one two three -> { one = one, two = two, three = three })
            |> Mrk.field "one" Mrk.string
            |> Mrk.field "two" Mrk.string
            |> Mrk.field "three" (Mrk.manyOf [ text ])
            |> Mrk.toBlock
        ]


floatDoc =
    Mrk.document
        [ Mrk.record "Test"
            (\one two three -> { one = one, two = two, three = three })
            |> Mrk.field "one" Mrk.float
            |> Mrk.field "two" Mrk.float
            |> Mrk.field "three" Mrk.float
            |> Mrk.toBlock
        ]


codeDoc =
    Mrk.document
        [ Mrk.block "Monospace"
            identity
            Mrk.string
        ]


singleOneOf =
    Mrk.document
        [ Mrk.block "Monospace"
            identity
            Mrk.string
        ]


codeAndTextDoc =
    Mrk.document
        [ Mrk.block "Monospace"
            identity
            Mrk.string
        , Mrk.map (always "text") text
        ]


intDoc =
    Mrk.document
        [ Mrk.record "Test"
            (\one two three -> { one = one, two = two, three = three })
            |> Mrk.field "one" Mrk.int
            |> Mrk.field "two" Mrk.int
            |> Mrk.field "three" Mrk.int
            |> Mrk.toBlock
        ]


sectionDoc =
    Mrk.document
        [ Mrk.map (always "text") text
        , Mrk.record "Test"
            (\one two three -> "record:one,two,three")
            |> Mrk.field "one" Mrk.string
            |> Mrk.field "two" Mrk.string
            |> Mrk.field "three" Mrk.string
            |> Mrk.toBlock
        , Mrk.block "Section"
            (\x -> "section:" ++ String.join "," x)
            (Mrk.manyOf
                [ Mrk.block "Embedded"
                    (always "embedded")
                    text
                , Mrk.map (always "text") text
                ]
            )
        ]


nested : Mrk.Document () (List Indexed)
nested =
    Mrk.document
        [ Mrk.block "Nested"
            identity
            (Mrk.tree
                identity
                (Mrk.map (always True) Mrk.int)
            )
            |> Mrk.map (renderEnumWith (renderIndex []))
        ]


nestedOrdering : Mrk.Document () (List Ordered)
nestedOrdering =
    Mrk.document
        [ Mrk.block "Nested"
            identity
            (Mrk.tree
                identity
                Mrk.int
            )
            |> Mrk.map (renderEnumWith (renderContent []))
        ]


type Indexed
    = Indexed Int (List Indexed)


type Ordered
    = Ordered (List Int) (List Ordered)


type Icons
    = Icons Mrk.Icon (List Icons)


renderEnumWith fn (Mrk.Enumerated enum) =
    List.indexedMap fn enum.items


renderEnumWithIcon fn (Mrk.Enumerated enum) =
    List.indexedMap (fn enum.icon) enum.items


renderContent stack i (Mrk.Item item) =
    Ordered item.content (renderEnumWith (renderContent (i :: stack)) item.children)


renderIndex stack i (Mrk.Item item) =
    Indexed i (renderEnumWith (renderIndex (i :: stack)) item.children)


renderIcon stack icon i (Mrk.Item node) =
    Icons icon (renderEnumWithIcon (renderIcon (i :: stack)) node.children)


iconListDoc : Mrk.Document () (List Icons)
iconListDoc =
    Mrk.document
        [ Mrk.block "WithIcons"
            identity
            (Mrk.tree
                identity
                Mrk.int
            )
            |> Mrk.map (renderEnumWithIcon (renderIcon []))
        ]


nestedString : Mrk.Document () (List Icons)
nestedString =
    Mrk.document
        [ Mrk.block "WithIcons"
            identity
            (Mrk.tree
                identity
                Mrk.string
            )
            |> Mrk.map (renderEnumWithIcon (renderIcon []))
        ]


simpleNestedOrderedDoc =
    """|> Nested
    --- 1
        2
    --- 3
        4
        5
    --- 6
    --- 7
"""


complexNestedOrderedDoc =
    """|> Nested
    --- 1
        2
    --- 3
        4
        --- 5
            6
            --- 12
        --- 7
            8
        --- 13
        --- 14
    --- 9
    --- 10
        --- 18

"""


simpleNestedDoc =
    """|> Nested
    - 1
    - 1
    - 1
"""


complexNestedDoc =
    """|> Nested
    - 1
        - 1
        - 1
        - 1
        - 1
            - 1
    - 1
        - 1
    - 1
"""


dedentingNestedDoc =
    """|> Nested
    - 1
        - 2
        - 3
        - 4
        - 1
            - 1
        - 1
            - 1
        - 1
    - 1
        - 1
    - 1
"""


iconSetting =
    """
|> WithIcons
    1. 1
        - 1
        - 1
        - 1
        - 1
            1. 1
            -- 1
    - 1
        - 1
    - 1
"""


emptyStyles =
    { bold = False
    , italic = False
    , strike = False
    }


sectionWithRecordDoc =
    Mrk.document
        [ text
            |> Mrk.map (always "text")
        , Mrk.record "Test"
            (\one two three -> "record:one,two,three")
            |> Mrk.field "one" Mrk.string
            |> Mrk.field "two" Mrk.string
            |> Mrk.field "three" Mrk.string
            |> Mrk.toBlock
        , Mrk.block "Section"
            (\x -> "embedded:" ++ String.join "," x)
            (Mrk.manyOf
                [ Mrk.block "Embedded"
                    (always "block")
                    text
                , text
                    |> Mrk.map (always "text")
                ]
            )
        ]


getProblem renderedError =
    case renderedError of
        Error.Rendered details ->
            details.problem

        Error.Global details ->
            details.problem


flattenErrors result =
    case result of
        Ok ( parsed, outcome ) ->
            outcome

        Err outcome ->
            outcome


toResult doc src =
    case flattenErrors (Mrk.Internal.Description.compile doc src) of
        Mrk.Internal.Outcome.Success ( _, success ) ->
            Ok success

        Mrk.Internal.Outcome.Failure errs ->
            Err (List.map getProblem errs)

        Mrk.Internal.Outcome.Almost { errors } ->
            Err (List.map getProblem errors)


suite : Test
suite =
    describe "Mark"
        [ describe "Text"
            [ -- test "Starts with Space" <|
              -- \_ ->
              --     Expect.equal
              --         (toResult toplevelText " one too many spaces...")
              --         (Err [ Error.CantStartTextWithSpace ])
              -- , test "Unclosed Italic" <|
              --     \_ ->
              --         Expect.equal
              --             (toResult toplevelText "/Start italics, but don't finish")
              --             (Err
              --              [ Error.Escape
              --              , Error.Expecting "/"
              --              , Error.Expecting "~"
              --              , Error.Expecting "*"
              --              , Error.InlineStart
              --              , Error.UnclosedStyles [ Mrk.Italic ]
              --              ]
              --             )
              test "Single newlines are allowed in paragraphs" <|
                \_ ->
                    Expect.equal
                        (toResult
                            (Mrk.document [ text ])
                            "Some text\nand some more text\n\n"
                        )
                        (Ok
                            [ [ ( emptyStyles, "Some text\nand some more text" )
                              ]
                            ]
                        )
            , test "Inline elements should maintain their source order." <|
                \_ ->
                    Expect.equal
                        (toResult inlines "[my]{highlight} highlighted [sentence]{highlight} [order]{highlight}")
                        (Ok
                            [ [ [ ( emptyStyles, "my" ) ]
                              , [ ( emptyStyles, " highlighted " ) ]
                              , [ ( emptyStyles, "sentence" ) ]
                              , [ ( emptyStyles, " " ) ]
                              , [ ( emptyStyles, "order" ) ]
                              ]
                            ]
                        )
            , test "Basic Verbatim Element." <|
                \_ ->
                    Expect.equal
                        (toResult inlines "my `verbatim string`{verb}")
                        (Ok
                            [ [ [ ( emptyStyles, "my " ) ]
                              , [ ( emptyStyles, "verbatim string" ) ]
                              ]
                            ]
                        )
            , test "Verbatim element without attributes" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "my `verbatim string`")
                        (Ok
                            [ [ [ Tuple.pair emptyStyles "my " ]
                              , [ Tuple.pair emptyStyles "verbatim string" ]
                              ]
                            ]
                        )
            , test "Verbatim element with attributes" <|
                \_ ->
                    Expect.equal
                        (toResult inlineMultipleVerbatim
                            "my `verbatim string`  `verbatim 2`{verb2|one=one, two=two}"
                        )
                        (Ok [ [ [ "my " ], [ "verbatim string" ], [ "  " ], [ "verbatim 2", "one", "two" ] ] ])
            , test "Inline attribute order(source order)" <|
                \_ ->
                    Expect.equal
                        (toResult inlineOrder "[test]{order|one = one, two = two}")
                        (Ok
                            [ [ Just ( "one", "two" )
                              ]
                            ]
                        )
            , test "Inline attribute order(not source order)" <|
                \_ ->
                    Expect.equal
                        (toResult inlineOrder "[test]{order|two = two, one = one}")
                        (Ok
                            [ [ Just ( "one", "two" )
                              ]
                            ]
                        )
            , test "Basic replacement" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "my test//text")
                        (Ok [ [ [ Tuple.pair emptyStyles "my test/text" ] ] ])
            , test "replace dash" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "my test--text")
                        (Ok [ [ [ Tuple.pair emptyStyles "my test–text" ] ] ])
            , test "Inline elements should maintain escaped italics" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "[my //]{highlight} highlighted [sentence]{highlight} [order]{highlight}")
                        (Ok
                            [ [ [ Tuple.pair emptyStyles "my /" ]
                              , [ Tuple.pair emptyStyles " highlighted " ]
                              , [ Tuple.pair emptyStyles "sentence" ]
                              , [ Tuple.pair emptyStyles " " ]
                              , [ Tuple.pair emptyStyles "order" ]
                              ]
                            ]
                        )
            , test "Inline elements should maintain multiple replacements" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "[my ////]{highlight} highlighted [sentence]{highlight} [order]{highlight}")
                        (Ok
                            [ [ [ Tuple.pair emptyStyles "my //" ]
                              , [ Tuple.pair emptyStyles " highlighted " ]
                              , [ Tuple.pair emptyStyles "sentence" ]
                              , [ Tuple.pair emptyStyles " " ]
                              , [ Tuple.pair emptyStyles "order" ]
                              ]
                            ]
                        )
            , test "Inline elements should maintain escaped characters" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "[my \\/]{highlight} highlighted [sentence]{highlight} [order]{highlight}")
                        (Ok
                            [ [ [ Tuple.pair emptyStyles "my /" ]
                              , [ Tuple.pair emptyStyles " highlighted " ]
                              , [ Tuple.pair emptyStyles "sentence" ]
                              , [ Tuple.pair emptyStyles " " ]
                              , [ Tuple.pair emptyStyles "order" ]
                              ]
                            ]
                        )
            , test "Incorrect inline element name" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "[my]{highlurt} highlighted sentence")
                        (Err
                            [ Error.UnknownInline
                                [ "[my]{highlight}"
                                , "`my`{verb}"
                                , "`my`{withColor| color = A String }"
                                ]
                            ]
                        )
            , test "Incorrect inline attribute content" <|
                \_ ->
                    Expect.equal
                        (toResult inlines "`my`{withColor | color = green } highlighted sentence")
                        (Err
                            [ Error.Custom
                                { message = [ "A color must be red or blue." ]
                                , title = "Bad Color"
                                }
                            ]
                        )
            ]
        , describe "Multiline"
            [ test "Correctly parse code block" <|
                \_ ->
                    Expect.equal
                        (toResult codeDoc """|> Monospace
    Here is my first line.
    Here is my second.
    Here is my third.
    Here is my fourth.
        And my indented line.

""")
                        (Ok [ "Here is my first line.\nHere is my second.\nHere is my third.\nHere is my fourth.\n    And my indented line." ])
            , test "Parse code block and then normal text" <|
                \_ ->
                    Expect.equal
                        (toResult codeAndTextDoc """|> Monospace
    Here is my first line.
    Here is my second.
    Here is my third.
    Here is my fourth.
        And my indented line.
Then some text.
""")
                        (Ok
                            [ "Here is my first line.\nHere is my second.\nHere is my third.\nHere is my fourth.\n    And my indented line."
                            , "text"
                            ]
                        )
            ]
        , describe "Nested"
            [ test "Simple list parsing.  No Nesting." <|
                \_ ->
                    Expect.equal
                        (toResult nested simpleNestedDoc)
                        (Ok
                            [ [ Indexed 0 []
                              , Indexed 1 []
                              , Indexed 2 []
                              ]
                            ]
                        )
            , test "Simple list parsing, maintains order" <|
                \_ ->
                    Expect.equal
                        (toResult nestedOrdering simpleNestedOrderedDoc)
                        (Ok
                            [ [ Ordered [ 1, 2 ] []
                              , Ordered [ 3, 4, 5 ] []
                              , Ordered [ 6 ] []
                              , Ordered [ 7 ] []
                              ]
                            ]
                        )
            , test "Complex list parsing, maintains order" <|
                \_ ->
                    Expect.equal
                        (toResult nestedOrdering complexNestedOrderedDoc)
                        (Ok
                            [ [ Ordered [ 1, 2 ] []
                              , Ordered [ 3, 4 ]
                                    [ Ordered [ 5, 6 ] [ Ordered [ 12 ] [] ], Ordered [ 7, 8 ] [], Ordered [ 13 ] [], Ordered [ 14 ] [] ]
                              , Ordered [ 9 ] []
                              , Ordered [ 10 ] [ Ordered [ 18 ] [] ]
                              ]
                            ]
                        )
            , test "Icons set by first element" <|
                \_ ->
                    Expect.equal
                        (toResult iconListDoc iconSetting)
                        (Ok
                            [ [ Icons Mrk.Number
                                    [ Icons Mrk.Bullet []
                                    , Icons Mrk.Bullet []
                                    , Icons Mrk.Bullet []
                                    , Icons Mrk.Bullet
                                        [ Icons Mrk.Number []
                                        , Icons Mrk.Number []
                                        ]
                                    ]
                              , Icons Mrk.Number [ Icons Mrk.Bullet [] ]
                              , Icons Mrk.Number []
                              ]
                            ]
                        )
            , test "Nested list parsing" <|
                \_ ->
                    Expect.equal
                        (toResult nested complexNestedDoc)
                        (Ok
                            [ [ Indexed 0
                                    [ Indexed 0 []
                                    , Indexed 1 []
                                    , Indexed 2 []
                                    , Indexed 3 [ Indexed 0 [] ]
                                    ]
                              , Indexed 1 [ Indexed 0 [] ]
                              , Indexed 2 []
                              ]
                            ]
                        )
            , test "Nested list dedenting correctly" <|
                \_ ->
                    Expect.equal
                        (toResult nested dedentingNestedDoc)
                        (Ok
                            [ [ Indexed 0
                                    [ Indexed 0 []
                                    , Indexed 1 []
                                    , Indexed 2 []
                                    , Indexed 3 [ Indexed 0 [] ]
                                    , Indexed 4 [ Indexed 0 [] ]
                                    , Indexed 5 []
                                    ]
                              , Indexed 1 [ Indexed 0 [] ]
                              , Indexed 2 []
                              ]
                            ]
                        )
            ]
        , describe "Blocks"
            [ test "Misspelled Block" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Turst
    Here's some text content
                        """
                    in
                    Expect.equal (toResult textDoc doc1)
                        (Err [ Error.UnknownBlock [ "Test" ] ])
            , test "Extra line between name and value" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
    Here's my extra line
"""

                        result =
                            Ok [ [ Tuple.pair emptyStyles "Here’s my extra line" ] ]
                    in
                    Expect.equal (toResult textDoc doc1)
                        result
            , test "Paragraph with single newline" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
    Here's my extra line
    And some additional stuff.
"""

                        result =
                            Ok [ [ Tuple.pair emptyStyles "Here’s my extra line\nAnd some additional stuff." ] ]
                    in
                    Expect.equal (toResult textDoc doc1)
                        result
            , test "Single OneOf" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Monospace
    Here's my extra line
"""
                    in
                    Expect.equal (toResult singleOneOf doc1)
                        (Ok [ "Here's my extra line" ])
            , test "Mulitline Text" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Monospace
    Here's my extra line
"""
                    in
                    Expect.equal (toResult codeDoc doc1)
                        (Ok [ "Here's my extra line" ])
            , test "Incorrect Indentation" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
  Only two spaces(should be four)

"""
                    in
                    Expect.equal (toResult textDoc doc1)
                        (Err [ Error.ExpectingIndent 4 ])
            , test "Start with Metadata" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Meta
    one = Test data
    two = other data

Then a bunch of

paragraphs.

Each with their own /styling/.
"""

                        result =
                            Ok
                                [ [ Tuple.pair emptyStyles "Then a bunch of"
                                  ]
                                , [ Tuple.pair emptyStyles "paragraphs." ]
                                , [ Tuple.pair emptyStyles "Each with their own "
                                  , Tuple.pair { bold = False, italic = True, strike = False } "styling"
                                  , Tuple.pair emptyStyles "."
                                  ]
                                ]
                    in
                    Expect.equal
                        (toResult withMetaData doc1)
                        result
            , test "Extra Newline to Start" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """
|> Meta
    one = Test data
    two = other data

Then a bunch of

paragraphs.

Each with their own /styling/.
"""
                    in
                    Expect.ok (toResult withMetaData doc1)
            , test "Nested section blocks" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """
|> Test
    one = Test data
    two = other data
    three = other test data

Then a bunch of

paragraphs.

Each with their own /styling/.

|> Section

    Then we have embedded stuff

    and we can add other blocks like

    |> Embedded
        This is embedded

    and others
Finally, a sentence
"""
                    in
                    Expect.equal (toResult sectionDoc doc1)
                        (Ok
                            [ "record:one,two,three"
                            , "text"
                            , "text"
                            , "text"
                            , "section:text,text,embedded,text"
                            , "text"
                            ]
                        )
            ]
        , describe "Error Correction"
            [ test "Verify an int" <|
                \_ ->
                    Expect.equal
                        (toResult (Mrk.document [ Mrk.int ]) "5")
                        (Ok [ 5 ])
            , test "Verify a range on an int" <|
                \_ ->
                    Expect.equal
                        (toResult
                            (Mrk.document
                                [ Mrk.int
                                    |> Mrk.verify
                                        (\x ->
                                            Ok x
                                        )
                                ]
                            )
                            "5"
                        )
                        (Ok [ 5 ])
            , test "Fail a range on an int" <|
                \_ ->
                    Expect.equal
                        (toResult
                            (Mrk.document
                                [ Mrk.int
                                    |> Mrk.verify
                                        (\x ->
                                            Err
                                                { title = "Out of range"
                                                , message =
                                                    []
                                                }
                                        )
                                ]
                            )
                            "5"
                        )
                        (Err [ Error.Custom { message = [], title = "Out of range" } ])
            ]
        , describe "Records"
            [ test "Missing fields should error" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
    one = hello
    two = world
"""

                        expectedProblem : Error.Error
                        expectedProblem =
                            Error.MissingFields [ "three" ]
                    in
                    Expect.equal (toResult recordDoc doc1)
                        (Err [ expectedProblem ])
            , test "Extra lines between fields" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
    one = hello
    two = world
    three = !
"""
                    in
                    Expect.equal
                        (toResult recordDoc doc1)
                        (Ok [ { one = "hello", three = "!", two = "world" } ])
            , test "Commas allowed in record fields" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
    one = hello, world
    two = how are you?
    three = !
"""
                    in
                    Expect.equal
                        (toResult recordDoc doc1)
                        (Ok [ { one = "hello, world", three = "!", two = "how are you?" } ])
            , test "Extra line between two fields" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
    one = hello
    two = world
    three = !
"""
                    in
                    Expect.equal (toResult recordDoc doc1)
                        (Ok [ { one = "hello", three = "!", two = "world" } ])
            , test "Records with many text as a field" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
    one = hello
    two = world
    three =
        Here is a bunch of text
"""
                    in
                    Expect.equal (toResult recordManyTextDoc doc1)
                        (Ok
                            [ { one = "hello"
                              , three = [ [ Tuple.pair emptyStyles "Here is a bunch of text" ] ]
                              , two = "world"
                              }
                            ]
                        )
            , test "Records with many text as a field (starting on same line)" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
    one = hello
    two = world
    three = Here is a bunch of text
"""
                    in
                    Expect.equal
                        (toResult recordManyTextDoc doc1)
                        (Ok
                            [ { one = "hello"
                              , three =
                                    [ [ Tuple.pair emptyStyles "Here is a bunch of text" ] ]
                              , two = "world"
                              }
                            ]
                        )
            , test "Records with multiple lines in field" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
    one = hello
    two = world
    three =
        Here is a bunch of text

        And some more on another line

"""
                    in
                    Expect.equal (toResult recordManyTextDoc doc1)
                        (Ok
                            [ { one = "hello"
                              , three =
                                    [ [ Tuple.pair emptyStyles "Here is a bunch of text" ]
                                    , [ Tuple.pair emptyStyles "And some more on another line" ]
                                    ]
                              , two = "world"
                              }
                            ]
                        )
            , test "Incorrect Indentation" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
  one = hello
  two = world
                        """
                    in
                    Expect.equal (toResult recordDoc doc1)
                        (Err [ Error.ExpectingIndent 4 ])
            , test "Incorrect Indentation v2" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
    one = hello
  two = world
                        """
                    in
                    Expect.equal (toResult recordDoc doc1)
                        (Err [ Error.ExpectingIndent 4 ])
            , test "Additional fields should error" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
    one = hello
    two = world
    three = Yo
    four = huh?

"""
                    in
                    Expect.equal (toResult recordDoc doc1)
                        (Err
                            [ Error.UnexpectedField
                                { options = [ "one", "two", "three" ]
                                , found = "four = huh?"
                                , recordName = "Test"
                                }
                            ]
                        )
            , test "Order of fields in source shouldn't matter" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
    one = hello
    two = world
    three = !
                        """

                        doc2 : String
                        doc2 =
                            """|> Test
    two = world
    one = hello
    three = !
                        """

                        parsed =
                            ( compile recordDoc doc1
                            , compile recordDoc doc2
                            )
                    in
                    Expect.all
                        [ \( one, two ) ->
                            Expect.equal one two
                        , \( one, two ) ->
                            Expect.true "Record parsing success"
                                (case one of
                                    Mrk.Success _ ->
                                        True

                                    _ ->
                                        False
                                )
                        ]
                        parsed
            , test "Floats are parsed as expected" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
    one = 15.25
    two = -1.0
    three = 2
                        """
                    in
                    Expect.equal (compile floatDoc doc1)
                        (Mrk.Success [ { one = 15.25, two = -1, three = 2 } ])
            , test "Ints are parsed as expected" <|
                \_ ->
                    let
                        doc1 : String
                        doc1 =
                            """|> Test
    one = 15
    two = -1
    three = 2
                        """
                    in
                    Expect.equal (compile intDoc doc1)
                        (Mrk.Success [ { one = 15, two = -1, three = 2 } ])
            ]
        ]


compile doc src =
    case Mrk.compile doc src of
        Mrk.Success ( _, data ) ->
            Mrk.Success data

        Mrk.Almost partial ->
            Mrk.Almost
                { errors = partial.errors
                , results = Tuple.second partial.result
                }

        Mrk.Failure fail ->
            Mrk.Failure fail
