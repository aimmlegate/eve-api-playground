module ESI exposing (getTypes)

import Decoders exposing (..)
import Http
import Json.Decode as Json exposing (..)
import Model exposing (..)
import Task exposing (Task)


handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Json.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result


query =
    "?datasource=tranquility&language=en-us"


getTypesId groupId =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://esi.evetech.net/latest/markets/groups/" ++ String.fromInt groupId ++ query
        , body = Http.emptyBody
        , timeout = Nothing
        , resolver = Http.stringResolver <| handleJsonResponse <| Decoders.typeListDecoder
        }


getTypesEntity groupId =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://esi.evetech.net/latest/universe/types/" ++ String.fromInt groupId ++ query
        , body = Http.emptyBody
        , timeout = Nothing
        , resolver = Http.stringResolver <| handleJsonResponse <| Decoders.typeDecoder
        }


getTypes id =
    getTypesId id
        |> Task.andThen
            (\ids ->
                List.map getTypesEntity ids |> Task.sequence
            )
