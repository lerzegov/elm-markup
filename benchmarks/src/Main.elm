module Main exposing (document, main, source)

import Benchmark exposing (..)
import BenchMrk.LowLevel
import BenchMrk.Runner exposing (BenchmarkProgram, program)
import Browser
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region
import Html
import Html.Attributes
import Mark
import Mrk.Default
import Mrk.Internal
import Task


type alias Model =
    { runs : List Run
    , toRun : List ( String, Int, BenchMrk.LowLevel.Operation )
    }


type Run
    = Run String Int Float


type Msg
    = NewResults String Int (Result BenchMrk.LowLevel.Error Float)


main =
    Browser.element
        { init =
            \() ->
                next
                    { runs = []
                    , toRun =
                        [ benchmark "3.0: AST -> Result" 100 newConverter
                        , benchmark "3.0: String -> Result" 100 newParser
                        , benchmark "2.0: String -> Result" 100 oldParser
                        ]
                    }
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


next model =
    case model.toRun of
        [] ->
            ( model, Cmd.none )

        upcoming :: rest ->
            ( { model | toRun = rest }
            , run upcoming
            )


benchmark a b c =
    ( a, b, BenchMrk.LowLevel.operation c )


run ( name, number, operation ) =
    Task.attempt
        (NewResults name number)
        (BenchMrk.LowLevel.sample number operation)


update msg model =
    case msg of
        NewResults name iterations result ->
            case result of
                Ok i ->
                    let
                        parsed =
                            Debug.log "result" (newParser ())
                    in
                    next
                        { model
                            | runs = Run name iterations i :: model.runs
                        }

                Err error ->
                    let
                        _ =
                            Debug.log "error" error
                    in
                    ( model, Cmd.none )


view model =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        ]
        (List.map viewResult model.runs)


viewResult (Run name iterations time) =
    Html.div []
        [ Html.text
            (name
                ++ " for "
                ++ String.fromInt iterations
                ++ " iterations at "
                ++ String.fromFloat (time / toFloat iterations)
                ++ "ms/call"
            )
        ]


oldParser _ =
    Mrk.parse document source


newParser _ =
    Mrk.Internal.compile newDocument source


newParsed =
    Mrk.Internal.parse newDocument source


newConverter _ =
    case newParsed of
        Mrk.Internal.Success pars ->
            let
                _ =
                    Mrk.Internal.convert newDocument pars
            in
            Element.none

        _ ->
            Element.none


{--}
source =
    """| Title
    My Article


Lorem Ipsum is simply--- dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's /standard/ dummy text ever since the 1500's, when an "unknown printer" took a galley of type and scrambled it to<>make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was *popularised* in the 1960's with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.

But, for real, here's a kitten.

| Image
    description = What a cute kitten.
    src = http://placekitten.com/g/200/300

| Header
    My section on lists


| Doodad
    pitch = 0.3
    adjustment = 50


What does a list look like?




Lorem Ipsum is simply--- dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's /standard/ dummy text ever since the 1500's, when an "unknown printer" took a galley of type and scrambled it to<>make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was *popularised* in the 1960's with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.

But, for real, here's a kitten.

| Image
    description = What a cute kitten.
    src = http://placekitten.com/g/200/300

| Header
    My section on lists


| Doodad
    pitch = 0.3
    adjustment = 50


What does a list look like?


Lorem Ipsum is simply--- dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's /standard/ dummy text ever since the 1500's, when an "unknown printer" took a galley of type and scrambled it to<>make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was *popularised* in the 1960's with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.

But, for real, here's a kitten.

| Image
    description = What a cute kitten.
    src = http://placekitten.com/g/200/300

| Header
    My section on lists


| Doodad
    pitch = 0.3
    adjustment = 50


What does a list look like?



Lorem Ipsum is simply--- dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's /standard/ dummy text ever since the 1500's, when an "unknown printer" took a galley of type and scrambled it to<>make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was *popularised* in the 1960's with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.

But, for real, here's a kitten.

| Image
    description = What a cute kitten.
    src = http://placekitten.com/g/200/300

| Header
    My section on lists


| Doodad
    pitch = 0.3
    adjustment = 50


What does a list look like?




 """


{-| Here we define our document.

This may seem a bit overwhelming, but 95% of it is copied directly from `Mrk.Default.document`. You can then customize as you see fit!

-}
document =
    let
        defaultText =
            Mrk.Default.textWith
                { code = Mrk.Default.defaultTextStyle.code
                , link = Mrk.Default.defaultTextStyle.link
                , inlines =
                    [ Mrk.inline "Drop"
                        (\txt model ->
                            Element.row [ Font.variant Font.smallCaps ]
                                (List.map (\item -> Mrk.Default.textFragment item model) txt)
                        )
                        |> Mrk.inlineText
                    ]
                , replacements = Mrk.Default.defaultTextStyle.replacements
                }
    in
    Mrk.document
        (\children model ->
            Element.textColumn
                [ Element.spacing 32
                , Element.padding 100
                , Element.centerX
                , Element.width (Element.px 900)
                ]
                (List.map (\v -> v model) children)
        )
        (Mrk.startWith
            (\myTitle myContent ->
                myTitle :: myContent
            )
            (Mrk.Default.title [ Font.size 48 ] defaultText)
            (Mrk.manyOf
                [ Mrk.Default.header [ Font.size 36 ] defaultText

                -- , Mrk.Default.list
                --     { style = \_ -> [ Element.spacing 16 ]
                --     , icon = Mrk.Default.listIcon
                --     }
                --     defaultText
                , Mrk.record2 "Doodad"
                    (\src description model ->
                        Element.text "doodad"
                    )
                    (Mrk.field "adjustment" (Mrk.intBetween 0 100))
                    (Mrk.field "pitch" (Mrk.floatBetween 0 1))
                , Mrk.Default.image []
                , Mrk.Default.monospace
                    [ Element.spacing 5
                    , Element.padding 24
                    , Background.color
                        (Element.rgba 0 0 0 0.04)
                    , Border.rounded 2
                    , Font.size 16
                    , Font.family
                        [ Font.external
                            { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                            , name = "Source Code Pro"
                            }
                        , Font.sansSerif
                        ]
                    ]

                -- Toplevel Text
                , Mrk.map (\viewEls model -> Element.paragraph [] (viewEls model)) defaultText
                ]
            )
        )


{-| Render a text fragment.
-}



-- textFragment : Mrk.Internal.Text -> model -> Element.Element msg


textFragment _ node model_ =
    case node of
        Mrk.Internal.Text s txt ->
            Element.el (List.concatMap toStyles s) (Element.text txt)


{-| -}
toStyles : Mrk.Internal.Style -> List (Element.Attribute msg)
toStyles style =
    case style of
        Mrk.Internal.Bold ->
            [ Font.bold ]

        Mrk.Internal.Italic ->
            [ Font.italic ]

        Mrk.Internal.Strike ->
            [ Font.strike ]


{-| Here we define our document.

This may seem a bit overwhelming, but 95% of it is copied directly from `Mrk.Default.document`. You can then customize as you see fit!

-}
newDocument =
    let
        defaultText =
            Mrk.Internal.map
                (\els model ->
                    List.map (\v -> v model) els
                )
                (Mrk.Internal.text
                    { error = always (\model -> Element.text "ugh")
                    , view = textFragment
                    , inlines =
                        [-- Mrk.Internal.inline "Link"
                         -- (\txt url model ->
                         --     Element.link [ Font.color (Element.rgb 0.8 0.8 0.9) ]
                         --         { url = url
                         --         , label =
                         --             Element.row [ Element.htmlAttribute (Html.Attributes.style "display" "inline-flex") ]
                         --                 (List.map (\item -> textFragment item model) txt)
                         --         }
                         -- )
                         -- |> Mrk.Internal.inlineText
                         -- |> Mrk.Internal.inlineString "url"
                        ]

                    -- [ link config.link
                    -- , code config.code
                    -- ]
                    --     ++ config.inlines
                    , replacements = []

                    -- Mrk.Default.defaultTextStyle.replacements
                    }
                )
    in
    Mrk.Internal.document
        (\pos children model ->
            Element.textColumn
                [ Element.spacing 32
                , Element.padding 100
                , Element.centerX
                , Element.width (Element.px 900)
                ]
                (List.map (\v -> v model) children)
        )
        (Mrk.Internal.startWith
            (\pos myTitle myContent ->
                myTitle :: myContent
            )
            (Mrk.Internal.block "Title"
                (\found model ->
                    viewOrError found <|
                        \elements ->
                            Element.paragraph
                                (Element.Region.heading 1 :: [ Font.size 48 ])
                                (elements model)
                )
                defaultText
            )
            (Mrk.Internal.manyOf
                (\_ model -> Element.text "Oh boy")
                [ --Mrk.Internal.Default.header [ Font.size 36 ] defaultText
                  Mrk.Internal.block "Header"
                    (\found model ->
                        viewOrError found <|
                            \elements ->
                                Element.paragraph
                                    (Element.Region.heading 2 :: [ Font.size 36 ])
                                    (elements model)
                    )
                    defaultText

                -- , Mrk.Internal.Default.list
                --     { style = \_ -> [ Element.spacing 16 ]
                --     , icon = Mrk.Internal.Default.listIcon
                --     }
                --     defaultText
                , Mrk.Internal.record2 "Doodad"
                    (\pos src description model ->
                        Element.text "doodad"
                    )
                    (\_ model -> Element.text "ugh, error")
                    (Mrk.Internal.field "adjustment" (Mrk.Internal.intBetween 0 100))
                    (Mrk.Internal.field "pitch" (Mrk.Internal.floatBetween 0 1))
                , Mrk.Internal.record2 "Image"
                    (\pos src description model ->
                        Element.image []
                            { src = src
                            , description = description
                            }
                    )
                    (\_ model -> Element.text "ugh, error")
                    (Mrk.Internal.field "src" Mrk.Internal.string)
                    (Mrk.Internal.field "description" Mrk.Internal.string)
                , Mrk.Internal.block "Monospace"
                    (\found model ->
                        viewOrError found <|
                            \string ->
                                Element.el
                                    (Element.htmlAttribute (Html.Attributes.style "line-height" "1.4em")
                                        :: Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
                                        :: [ Element.spacing 5
                                           , Element.padding 24
                                           , Background.color
                                                (Element.rgba 0 0 0 0.04)
                                           , Border.rounded 2
                                           , Font.size 16
                                           , Font.family
                                                [ Font.external
                                                    { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                                                    , name = "Source Code Pro"
                                                    }
                                                , Font.sansSerif
                                                ]
                                           ]
                                    )
                                    (Element.text (String.trimRight string))
                    )
                    Mrk.Internal.multiline

                -- Toplevel Text
                , Mrk.Internal.map (\viewEls model -> Element.paragraph [] (viewEls model)) defaultText
                ]
            )
        )


viewOrError found successView =
    case found.found of
        Mrk.Internal.Found _ x ->
            successView x

        Mrk.Internal.Unexpected unexpected ->
            Element.text "oh dang"
