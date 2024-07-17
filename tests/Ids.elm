module Ids exposing (..)

{-| -}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Mrk
import Mrk.Edit
import Mrk.Internal.Description as Description
import Mrk.Internal.Error as Error
import Mrk.Internal.Id as Id
import Mrk.New
import Test exposing (..)


sectionDoc =
    Mrk.document
        [ Mrk.withId (\i b -> [ Tuple.pair i b ]) <|
            Mrk.map (String.join ":" << List.map Tuple.second) text
        , idRecord
        , Mrk.withId
            (\x y ->
                ( x, "section block" ) :: y
            )
          <|
            Mrk.block "Section"
                List.concat
                (Mrk.manyOf
                    [ Mrk.withId
                        (\x y ->
                            ( x, "embedded block" ) :: y
                        )
                      <|
                        Mrk.block "Embedded"
                            identity
                            (Mrk.withId (\i b -> [ Tuple.pair i ("!!" ++ b) ]) <|
                                Mrk.map (String.join ":" << List.map Tuple.second) text
                            )
                    , Mrk.withId (\i b -> [ Tuple.pair i b ]) <|
                        Mrk.map (String.join ":" << List.map Tuple.second) text
                    ]
                )
        , Mrk.withId
            (\x y -> ( x, "list block" ) :: y)
            list
        ]


idRecord =
    Mrk.withId
        (\i b -> ( i, "record main!" ) :: b)
        (Mrk.record "Test"
            (\one two three ->
                [ one
                , two
                , three
                ]
            )
            |> Mrk.field "one" (Mrk.withId Tuple.pair Mrk.string)
            |> Mrk.field "two" (Mrk.withId Tuple.pair Mrk.string)
            |> Mrk.field "three" (Mrk.withId Tuple.pair Mrk.string)
            |> Mrk.toBlock
        )


list : Mrk.Block (List ( Id.Id, String ))
list =
    Mrk.block "List"
        identity
        (Mrk.tree
            renderList
            (Mrk.withId (\i b -> [ Tuple.pair i b ]) <|
                Mrk.map (String.join "--" << List.map Tuple.second) text
            )
        )


{-| Note: we have to define this as a separate function because
`Enumerated` and `Item` are a pair of mutually recursive data structures.
It's easiest to render them using two separate functions: renderList and renderItem
-}
renderList : Mrk.Enumerated (List ( Id.Id, String )) -> List ( Id.Id, String )
renderList (Mrk.Enumerated enum) =
    List.concatMap renderItem enum.items


renderItem : Mrk.Item (List ( Id.Id, String )) -> List ( Id.Id, String )
renderItem (Mrk.Item item) =
    case item.children of
        Mrk.Enumerated enum ->
            case enum.items of
                [] ->
                    List.concat item.content

                _ ->
                    List.concat item.content ++ renderList item.children


text =
    Mrk.text Tuple.pair


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


|> List
    1.  This is definitely the first thing.
        Add all together now
        With some Content

    -- Another thing.

        1. sublist

        -- more sublist

            -- indented


        -- other sublist

            -- subthing

            -- other subthing

    -- and yet, another
        --  and another one
            With some content



Each with their own /styling/.

|> Section

    Then we have embedded stuff

    and we can add other blocks like

    |> Embedded
        This is embedded

    and others
Finally, a sentence

"""


getProblem renderedError =
    case renderedError of
        Error.Rendered details ->
            details.problem

        Error.Global details ->
            details.problem


toResult doc src =
    case Mrk.compile doc src of
        Mrk.Success ( _, success ) ->
            Ok success

        Mrk.Failure errs ->
            Err (List.map getProblem errs)

        Mrk.Almost { errors } ->
            Err (List.map getProblem errors)


suite =
    describe "ID handling"
        [ test "IDs are not duplicated" <|
            \_ ->
                case toResult sectionDoc doc1 of
                    Err errs ->
                        let
                            _ =
                                Debug.log "error" errs
                        in
                        Expect.fail "Document failed to parse for tests"

                    Ok data ->
                        let
                            stringIds =
                                List.concatMap (List.map (Mrk.idToString << Tuple.first)) data

                            -- _ =
                            --     List.map (Debug.log "ids") data
                        in
                        Expect.true "All IDs found are unique"
                            (stringIds
                                |> List.all
                                    (\str ->
                                        List.filter (\s -> s == str) stringIds
                                            |> List.length
                                            |> (\len -> len == 1)
                                    )
                            )
        ]
