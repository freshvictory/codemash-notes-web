module Page.Home exposing (Model, Msg, init, update, view)

import Api exposing (Note)
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled
    exposing
        ( Html
        , button
        , div
        , h1
        , header
        , span
        , text
        , form
        , textarea
        )
import Html.Styled.Attributes exposing (css, type_)
import Html.Styled.Events as Events
import Http
import Page



-- MODEL


type alias Model =
    { notes : Dict Int Note
    , requestState : RequestState
    , editState : EditState
    , confirm : ConfirmState
    }


type RequestState
    = Loading
    | Success
    | Failure String


type EditState
    = NotEditing
    | Editing Note


type ConfirmState
    = Hidden
    | Active Int


init : ( Model, Cmd Msg )
init =
    ( { notes = Dict.empty
      , requestState = Loading
      , editState = NotEditing
      , confirm = Hidden
      }
    , Api.getNotes ReceivedNotes
    )



-- UPDATE


type Msg
    = ReceivedNotes (Result Http.Error (List Note))
    | StartEditing Note
    | Save
    | StopEditing
    | SavedNote (Result Http.Error Note)
    | OnNoteChange Note
    | ShowDeleteConfirm Int
    | Delete Int
    | NoteDeleted Int (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedNotes result ->
            case result of
                Ok notes ->
                    ( { model
                        | notes = List.foldl (\n -> Dict.insert n.id n) Dict.empty notes
                        , requestState = Success
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | requestState = Failure "Something went wrong" }, Cmd.none )

        StartEditing note ->
            ( { model | editState = Editing note }, Cmd.none )

        OnNoteChange note ->
            ( { model | editState = Editing note }, Cmd.none )

        StopEditing ->
            ( { model | editState = NotEditing }, Cmd.none )

        Save ->
            case model.editState of
                NotEditing ->
                    ( model, Cmd.none )
                Editing note ->
                    ( { model | editState = NotEditing }, Api.saveNote SavedNote note )

        SavedNote result ->
            case result of
                Ok note ->
                    ( { model | notes = Dict.insert note.id note model.notes }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ShowDeleteConfirm id ->
            ( { model | confirm = Active id }, Cmd.none )

        Delete id ->
            ( model, Api.deleteNote (NoteDeleted id) id )

        NoteDeleted id result ->
            case result of
                Ok _ ->
                    ( { model | notes = Dict.remove id model.notes }, Cmd.none )
                Err _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Page.Details Msg
view model =
    { title = "Home"
    , attrs = []
    , body =
        [ viewHeader
        , viewContent model
        ]
    }


viewHeader : Html Msg
viewHeader =
    header
        [ css
            [ padding (px 20)
            , borderBottom3 (px 2) solid (hex "AAA")
            ]
        ]
        [ div
            []
            [ h1
                [ css
                    [ margin zero
                    ]
                ]
                [ text "CodeMash Notes" ]
            ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    div
        [ css
            [ margin (px 20)
            ]
        ]
        [ case model.requestState of
            Success ->
                viewNotes model

            Failure err ->
                text err

            Loading ->
                text "Loading notes..."
        ]


viewNotes : Model -> Html Msg
viewNotes model =
    div
        []
        (model.notes
            |> Dict.map (\_ -> viewNoteSummary model)
            |> Dict.values
        )


viewNoteSummary : Model -> Note -> Html Msg
viewNoteSummary model note =
    case model.editState of
        Editing n ->
            if n.id == note.id then
                viewNoteEdit n

            else
                viewNoteDefault note

        _ ->
            viewNoteDefault note


viewNoteDefault : Note -> Html Msg
viewNoteDefault note =
    div
        [ css
            [ padding (px 15)
            , border3 (px 2) solid (hex "AAA")
            , borderRadius (px 10)
            , marginBottom (px 20)
            ]
        ]
        [ div
            [ css
                [ marginBottom (px 10)
                , paddingBottom (px 5)
                , borderBottom3 (px 1) solid (hex "CCC")
                ]
            ]
            [ span
                [ css
                    [ fontWeight bold
                    ]
                ]
                [ text note.title ]
            , span
                [ css
                    [ whiteSpace pre
                    ]
                ]
                [ text " • " ]
            , span
                [ css
                    [ color (hex "555")
                    ]
                ]
                [ text note.presenter ]
            , span
                [ css
                    [ whiteSpace pre
                    ]
                ]
                [ text " • " ]
            , span
                [ css
                    [ color
                        (if note.rating > 7 then
                            hex "50b946"

                         else if note.rating > 5 then
                            hex "fca403"

                         else
                            hex "ff0000"
                        )
                    ]
                ]
                [ text (String.fromInt note.rating ++ "/10") ]
            , div
                [ css
                    [ float right
                    , displayFlex
                    , color (hex "555")
                    , fontSize small
                    ]
                ]
                [ button
                    [ css
                        [ margin zero
                        ]
                    , Events.onClick (StartEditing note)
                    ]
                    [ text "edit" ]
                , div
                    [ css
                        [ whiteSpace pre
                        ]
                    ]
                    [ text " | " ]
                , button
                    [ css
                        [ margin zero
                        ]
                    , Events.onClick (Delete note.id)
                    ]
                    [ text "delete" ]
                ]
            ]
        , div
            []
            [ text note.note
            ]
        ]


viewNoteEdit : Note -> Html Msg
viewNoteEdit note =
    form
        [ css
            [ padding (px 15)
            , border3 (px 2) solid (hex "AAA")
            , borderRadius (px 10)
            , marginBottom (px 20)
            ]
        , Events.onSubmit Save
        ]
        [ div
            [ css
                [ marginBottom (px 10)
                , paddingBottom (px 5)
                , borderBottom3 (px 1) solid (hex "CCC")
                ]
            ]
            [ span
                [ css
                    [ fontWeight bold
                    ]
                ]
                [ text note.title ]
            , span
                [ css
                    [ whiteSpace pre
                    ]
                ]
                [ text " • " ]
            , span
                [ css
                    [ color (hex "555")
                    ]
                ]
                [ text note.presenter ]
            , span
                [ css
                    [ whiteSpace pre
                    ]
                ]
                [ text " • " ]
            , span
                [ css
                    [ color
                        (if note.rating > 7 then
                            hex "50b946"

                         else if note.rating > 5 then
                            hex "fca403"

                         else
                            hex "ff0000"
                        )
                    ]
                ]
                [ text (String.fromInt note.rating ++ "/10") ]
            , div
                [ css
                    [ float right
                    , displayFlex
                    , color (hex "555")
                    , fontSize small
                    ]
                ]
                [ button
                    [ css
                        [ margin zero
                        ]
                    , type_ "submit"
                    ]
                    [ text "save" ]
                , div
                    [ css
                        [ whiteSpace pre
                        ]
                    ]
                    [ text " | " ]
                , button
                    [ css
                        [ margin zero
                        ]
                    , Events.onClick StopEditing
                    , type_ "button"
                    ]
                    [ text "cancel" ]
                ]
            ]
        , textarea
            [ css
                [ width (pct 100)
                , padding (px 5)
                , margin zero
                , boxSizing borderBox
                , border3 (px 1) solid (hex "AAA")
                , borderRadius (px 5)
                , fontSize inherit
                ]
            , Events.onInput (\s -> OnNoteChange { note | note = s })
            ]
            [ text note.note
            ]
        ]
