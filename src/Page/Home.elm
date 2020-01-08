module Page.Home exposing (Model, Msg, init, update, view)

import Api exposing (Note)
import Html.Styled
    exposing
        ( Html
        , div
        , header
        , nav
        , text
        )
import Http
import Page



-- MODEL


type alias Model =
    { notes : List Note
    , state : State
    }


type State
    = Loading
    | Success
    | Failure String


init : ( Model, Cmd Msg )
init =
    ( { notes = []
      , state = Loading
      }
    , Api.getNotes ReceivedNotes
    )



-- UPDATE


type Msg
    = ReceivedNotes (Result Http.Error (List Note))


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ReceivedNotes result ->
            case result of
                Ok notes ->
                    ( { model | notes = notes, state = Success }, Cmd.none )

                Err _ ->
                    ( { model | state = Failure "Something went wrong" }, Cmd.none )



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
        []
        [ nav
            []
            []
        ]


viewContent : Model -> Html Msg
viewContent model =
    div
        []
        [ case model.state of
            Success ->
                viewNotes model.notes

            Failure err ->
                text err

            Loading ->
                text "Loading notes..."
        ]


viewNotes : List Note -> Html Msg
viewNotes notes =
    div
        []
        (List.map viewNoteSummary notes)


viewNoteSummary : Note -> Html Msg
viewNoteSummary note =
    div
        []
        [ text note.title
        ]
