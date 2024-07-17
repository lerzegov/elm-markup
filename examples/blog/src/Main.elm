module Main exposing (document, main)

{-| A /very/ simple blog post with a custom inline element for some cool text formatting.

This is to get you started.

-}

import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Http
import Mark
import Mrk.Error


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init () =
    ( { source = Nothing }
    , Http.get
        { url = "/articles/Ipsum.emu"
        , expect = Http.expectString GotSrc
        }
    )


type alias Model =
    { source : Maybe String
    }


type Msg
    = GotSrc (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSrc result ->
            case result of
                Ok src ->
                    ( { model | source = Just src }
                    , Cmd.none
                    )

                Err err ->
                    let
                        _ =
                            Debug.log "err" err
                    in
                    ( model, Cmd.none )


view model =
    { title = ""
    , body =
        [ case model.source of
            Nothing ->
                Html.text "Source not received yet"

            Just source ->
                case Mrk.compile document source of
                    Mrk.Success html ->
                        Html.div [] html.body

                    Mrk.Almost { result, errors } ->
                        -- This is the case where there has been an error,
                        -- but it has been caught by `Mrk.onError` and is still rendereable.
                        Html.div []
                            [ Html.div [] (viewErrors errors)
                            , Html.div [] result.body
                            ]

                    Mrk.Failure errors ->
                        Html.div []
                            (viewErrors errors)
        ]
    }


viewErrors errors =
    List.map
        (Mrk.Error.toHtml Mrk.Error.Light)
        errors


stylesheet =
    """
@import url('https://fonts.googleapis.com/css?family=EB+Garamond');
.italic {
    font-style: italic;
}
.bold {
    font-weight: bold;
}
.strike {
    text-decoration: line-through;
}
body {
    font-family: 'EB Garamond', serif;
    font-size: 20px;
    width: 600px;
    margin-left:auto;
    margin-right:auto;
    padding: 48px 0;
}
.drop-capital {
    font-size: 2.95em;
    line-height: 0.89em;
    float:left;
    margin-right: 8px;
}
.lede {
    font-variant: small-caps;
    margin-left: -15px;
}

"""


document =
    Mrk.documentWith
        (\meta body ->
            { metadata = meta
            , body =
                Html.node "style" [] [ Html.text stylesheet ]
                    :: Html.h1 [] meta.title
                    :: body
            }
        )
        -- We have some required metadata that starts our document.
        { metadata = metadata
        , body =
            Mrk.manyOf
                [ header
                , image
                , list
                , code
                , Mrk.map (Html.p []) text
                ]
        }



{- Handle Text -}


text =
    Mrk.textWith
        { view =
            \styles string ->
                viewText styles string
        , replacements = Mrk.commonReplacements
        , inlines =
            [ Mrk.annotation "link"
                (\texts url ->
                    Html.a [ Attr.href url ] (List.map (applyTuple viewText) texts)
                )
                |> Mrk.field "url" Mrk.string
            , Mrk.verbatim "drop"
                (\str ->
                    let
                        drop =
                            String.left 1 str

                        lede =
                            String.dropLeft 1 str
                    in
                    Html.span []
                        [ Html.span [ Attr.class "drop-capital" ]
                            [ Html.text drop ]
                        , Html.span [ Attr.class "lede" ]
                            [ Html.text lede ]
                        ]
                )
            ]
        }


applyTuple fn ( one, two ) =
    fn one two


viewText styles string =
    if styles.bold || styles.italic || styles.strike then
        Html.span
            [ Attr.classList
                [ ( "bold", styles.bold )
                , ( "italic", styles.italic )
                , ( "strike", styles.strike )
                ]
            ]
            [ Html.text string ]

    else
        Html.text string



{- Handle Metadata -}


metadata =
    Mrk.record "Article"
        (\author description title ->
            { author = author
            , description = description
            , title = title
            }
        )
        |> Mrk.field "author" Mrk.string
        |> Mrk.field "description" text
        |> Mrk.field "title" text
        |> Mrk.toBlock



{- Handle Blocks -}


header =
    Mrk.block "H1"
        (\children ->
            Html.h1 []
                children
        )
        text


image =
    Mrk.record "Image"
        (\src description ->
            Html.img
                [ Attr.src src
                , Attr.alt description
                , Attr.style "float" "left"
                , Attr.style "margin-right" "48px"
                ]
                []
        )
        |> Mrk.field "src" Mrk.string
        |> Mrk.field "description" Mrk.string
        |> Mrk.toBlock


code =
    Mrk.block "Code"
        (\str ->
            Html.pre
                [ Attr.style "padding" "12px"
                , Attr.style "background-color" "#eee"
                ]
                [ Html.text str ]
        )
        Mrk.string



{- Handling bulleted and numbered lists -}


list : Mrk.Block (Html msg)
list =
    Mrk.tree "List" renderList (Mrk.map (Html.div []) text)



-- Note: we have to define this as a separate function because
-- `Items` and `Node` are a pair of mutually recursive data structures.
-- It's easiest to render them using two separate functions:
-- renderList and renderItem


renderList : Mrk.Enumerated (Html msg) -> Html msg
renderList (Mrk.Enumerated enum) =
    let
        group =
            case enum.icon of
                Mrk.Bullet ->
                    Html.ul

                Mrk.Number ->
                    Html.ol
    in
    group []
        (List.map renderItem enum.items)


renderItem : Mrk.Item (Html msg) -> Html msg
renderItem (Mrk.Item item) =
    Html.li []
        [ Html.div [] item.content
        , renderList item.children
        ]
