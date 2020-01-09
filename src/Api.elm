module Api exposing (Note, getNotes, saveNote, deleteNote)

import Http
import Json.Decode exposing (Decoder, field)
import Json.Encode


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
        { url = "http://localhost:8080/api/notes"
        , expect = Http.expectJson m (Json.Decode.list noteDecoder)
        }


saveNote : (Result Http.Error Note -> msg) -> Note -> Cmd msg
saveNote m note =
    put
        { url = "http://localhost:8080/api/notes/" ++ String.fromInt note.id
        , body = Http.jsonBody (noteEncoder note)
        , expect = Http.expectJson m noteDecoder
        }


deleteNote : (Result Http.Error () -> msg) -> Int -> Cmd msg
deleteNote m id =
    delete
        { url = "http://localhost:8080/api/notes/" ++ String.fromInt id
        , expect = Http.expectWhatever m
        }
    


noteDecoder : Decoder Note
noteDecoder =
    Json.Decode.map5 Note
        (field "id" Json.Decode.int)
        (field "title" Json.Decode.string)
        (field "presenter" Json.Decode.string)
        (field "rating" Json.Decode.int)
        (field "notes" Json.Decode.string)


noteEncoder : Note -> Json.Encode.Value
noteEncoder note =
    Json.Encode.object
        [ ( "id", Json.Encode.int note.id )
        , ( "title", Json.Encode.string note.title )
        , ( "presenter", Json.Encode.string note.presenter )
        , ( "rating", Json.Encode.int note.rating )
        , ( "notes", Json.Encode.string note.note )
        ]


put : { url : String, body : Http.Body, expect : Http.Expect msg } -> Cmd msg
put request =
    Http.request
        { method = "PUT"
        , headers = []
        , url = request.url
        , body = request.body
        , expect = request.expect
        , timeout = Nothing
        , tracker = Nothing
        }


delete : { url : String, expect : Http.Expect msg } -> Cmd msg
delete request =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = request.url
        , body = Http.emptyBody
        , expect = request.expect
        , timeout = Nothing
        , tracker = Nothing
        }
