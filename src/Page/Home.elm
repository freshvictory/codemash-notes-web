module Page.Home exposing (Model, Msg, init, update, view)

import Api exposing (Note)
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled
    exposing
        ( Html
        , button
        , div
        , form
        , h1
        , header
        , input
        , label
        , option
        , select
        , span
        , text
        , textarea
        )
import Html.Styled.Attributes exposing (css, for, id, type_, value)
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


defaultNote : Note
defaultNote =
    { id = 0
    , title = ""
    , presenter = ""
    , note = ""
    , rating = 1
    }


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
    | AddNote
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
                Editing note ->
                    ( { model | editState = NotEditing }
                    , (if note.id == 0 then Api.addNote else Api.saveNote) SavedNote note
                    )

                _ ->
                    ( model, Cmd.none )

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

        AddNote ->
            ( { model | editState = Editing defaultNote }, Cmd.none )



-- VIEW


view : Model -> Page.Details Msg
view model =
    { title = "Home"
    , attrs = []
    , body =
        [ viewHeader model
        , viewContent model
        ]
    }


viewHeader : Model -> Html Msg
viewHeader model =
    header
        [ css
            [ padding (px 20)
            , borderBottom3 (px 2) solid (hex "AAA")
            , displayFlex
            , justifyContent spaceBetween
            , alignItems center
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
        , case model.editState of
            Editing n ->
                if n.id /= 0 then
                    viewAddNotesButton
                else
                    text ""

            _ ->
                viewAddNotesButton
        ]


viewAddNotesButton : Html Msg
viewAddNotesButton =
    button
        [ css
            [ color (hex "555")
            , border3 (px 1) solid (hex "555")
            , borderRadius (px 5)
            , padding (px 5)
            ]
        , Events.onClick AddNote
        ]
        [ text "add notes" ]


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
    let
        noteEntries =
            case model.editState of
                Editing n ->
                    if n.id == 0 then [ viewNoteForm n ] else []
                _ -> []
    in
        div
            []
            ( List.concat
                [ noteEntries
                , model.notes
                    |> Dict.map (\_ -> viewNoteSummary model)
                    |> Dict.values
                ]
            )


viewNoteSummary : Model -> Note -> Html Msg
viewNoteSummary model note =
    case model.editState of
        Editing n ->
            if n.id == note.id then
                viewNoteForm n

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
                , displayFlex
                , alignItems flexEnd
                ]
            ]
            [ div
                [ css
                    [ display inlineFlex
                    , flexWrap wrap
                    , marginRight (px 15)
                    ]
                ]
                [ span
                    [ css
                        [ fontWeight bold
                        ]
                    ]
                    [ text note.title
                    , span
                        [ css
                            [ margin2 zero (px 5)
                            , fontWeight normal
                            ]
                        ]
                        [ text "•" ]
                    ]
                , span
                    [ css
                        [ color (hex "555")
                        ]
                    ]
                    [ text note.presenter
                    , span
                        [ css
                            [ margin2 zero (px 5)
                            , color (hex "000")
                            ]
                        ]
                        [ text "•" ]
                    ]
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
                ]
            , div
                [ css
                    [ marginLeft auto
                    , maxWidth maxContent
                    , display inlineFlex
                    , color (hex "555")
                    , fontSize small
                    ]
                ]
                [ button
                    [ css
                        [ margin zero
                        , marginRight (px 7)
                        ]
                    , Events.onClick (StartEditing note)
                    ]
                    [ text "edit" ]
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


viewNoteForm : Note -> Html Msg
viewNoteForm note =
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
                [ marginLeft auto
                , displayFlex
                , maxWidth maxContent
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
        , div
            [ css
                [ displayFlex
                , flexDirection column
                , alignItems flexStart
                ]
            ]
            [ formInput "title" "Session title" note.title (\s -> OnNoteChange { note | title = s })
            , formInput "presenter" "Presenter" note.presenter (\s -> OnNoteChange { note | presenter = s })
            , noteRatingInput note
            , noteTextInput note
            ]
        ]


noteRatingInput : Note -> Html Msg
noteRatingInput note =
    div
        [ css
            [ display inlineFlex
            , margin2 (px 10) zero
            , position relative
            , padding (px 10)
            , border3 (px 1) solid (hex "AAA")
            , borderRadius (px 5)
            , alignItems center
            ]
        ]
        [ label
            [ for "rating"
            , css
                [ position absolute
                , backgroundColor (hex "FFF")
                , fontSize (px 12)
                , lineHeight (num 1)
                , top (px -6)
                , left (px 12)
                , padding2 zero (px 5)
                ]
            ]
            [ text "Rating" ]
        , select
            [ css
                [ margin zero
                , marginRight (px 5)

                -- , padding zero
                -- , paddingLeft (px 10)
                -- , border zero
                ]
            , id "rating"

            -- , type_ "number"
            -- , value (String.fromInt note.rating)
            -- , Html.Styled.Attributes.max "10"
            -- , Html.Styled.Attributes.min "1"
            , Events.onInput
                (\s ->
                    case String.toInt s of
                        Just i ->
                            OnNoteChange { note | rating = i }

                        _ ->
                            NoOp
                )
            ]
            (List.map
                (\n ->
                    let
                        s =
                            String.fromInt n
                    in
                    option
                        [ value s
                        , Html.Styled.Attributes.selected (n == note.rating)
                        ]
                        [ text s ]
                )
                (List.range 1 10)
            )
        , text "/ 10"
        ]


formInput : String -> String -> String -> (String -> Msg) -> Html Msg
formInput identifier textLabel valueText onInputMsg =
    div
        [ css
            [ position relative
            , margin2 (px 10) zero
            , display inlineBlock
            ]
        ]
        [ label
            [ for identifier
            , css
                [ position absolute
                , backgroundColor (hex "FFF")
                , fontSize (px 12)
                , lineHeight (num 1)
                , top (px -3)
                , left (px 12)
                , padding2 zero (px 5)
                ]
            ]
            [ text textLabel ]
        , input
            [ id identifier
            , value valueText
            , Events.onInput onInputMsg
            , css
                [ border3 (px 1) solid (hex "AAA")
                , borderRadius (px 5)
                , padding (px 10)
                ]
            ]
            []
        ]


noteTextInput : Note -> Html Msg
noteTextInput note =
    div
        [ css
            [ marginTop (px 10)
            , position relative
            , width (pct 100)
            ]
        ]
        [ label
            [ for "note"
            , css
                [ position absolute
                , backgroundColor (hex "FFF")
                , fontSize (px 12)
                , lineHeight (num 1)
                , top (px -3)
                , left (px 12)
                , padding2 zero (px 5)
                ]
            ]
            [ text "Note text" ]
        , textarea
            [ css
                [ width (pct 100)
                , padding (px 10)
                , boxSizing borderBox
                , border3 (px 1) solid (hex "AAA")
                , borderRadius (px 5)
                , fontSize inherit
                ]
            , id "note"
            , Events.onInput (\s -> OnNoteChange { note | note = s })
            ]
            [ text note.note
            ]
        ]
