module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Mark
import Mrk.Edit as Edit


type alias Model =
    { parsed : Maybe Mrk.Parsed }


document : Mrk.Document (Html Msg)
document =
    Mrk.document
        (\html -> div [] [ html ])
        (Mrk.block "Title"
            (\( id, s ) ->
                h1 [ onClick (Click id) ] [ text s ]
            )
            editableText
        )


editableText =
    Mrk.withId Tuple.pair Mrk.string


initialModel : Model
initialModel =
    { parsed =
        case Mrk.parse document "|> Title \n    some text" of
            Mrk.Success parsed ->
                Just parsed

            _ ->
                Nothing
    }


type Msg
    = Click Edit.Id


update : Msg -> Model -> Model
update (Click id) model =
    case model.parsed of
        Just parsed ->
            case Edit.update document (Edit.deleteText id 0 5) parsed of
                Ok newParsed ->
                    { model | parsed = Just newParsed }

                Err errors ->
                    let
                        e =
                            Debug.log "errors" errors
                    in
                    model

        _ ->
            model


view : Model -> Html Msg
view model =
    case model.parsed of
        Nothing ->
            div [] []

        Just parsed ->
            case Mrk.render document parsed of
                Mrk.Success html ->
                    html

                _ ->
                    div [] []


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
