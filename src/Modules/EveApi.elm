module EveApi exposing (getTypes)

import Http
import Json.Decode as Json exposing (..)
import Model exposing (..)
import Task exposing (Task)


typeListDecoder =
    field "types" (Json.list Json.int)


typeDecoder =
    Json.map5 Type
        (field "description" Json.string)
        (field "name" Json.string)
        (field "market_group_id" Json.int)
        (field "group_id" Json.int)
        (field "type_id" Json.int)


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


getTypesId groupId =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://esi.evetech.net/latest/markets/groups/" ++ String.fromInt groupId
        , body = Http.emptyBody
        , timeout = Nothing
        , resolver = Http.stringResolver <| handleJsonResponse <| typeListDecoder
        }


getTypesEntity groupId =
    Http.task
        { method = "GET"
        , headers = []
        , url = "https://esi.evetech.net/latest/universe/types/" ++ String.fromInt groupId
        , body = Http.emptyBody
        , timeout = Nothing
        , resolver = Http.stringResolver <| handleJsonResponse <| typeDecoder
        }


getTypes id =
    getTypesId id
        |> Task.andThen
            (\ids ->
                List.map getTypesEntity ids |> Task.sequence
            )
