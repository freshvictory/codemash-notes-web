module Api exposing (Note, getNotes)

import Http
import Json.Decode exposing (Decoder, field)


type alias Note =
    { id : Int
    , title : String
    , presenter : String
    , rating : Int
    , note : String
    }


getNotes : (Result Http.Error (List Note) -> msg) -> Cmd msg
getNotes m =
    Http.get
        { url = "https://swift-notes-api.herokuapp.com/api/notes"
        , expect = Http.expectJson m (Json.Decode.list noteDecoder)
        }


noteDecoder : Decoder Note
noteDecoder =
    Json.Decode.map5 Note
        (field "id" Json.Decode.int)
        (field "title" Json.Decode.string)
        (field "presenter" Json.Decode.string)
        (field "rating" Json.Decode.int)
        (field "notes" Json.Decode.string)
