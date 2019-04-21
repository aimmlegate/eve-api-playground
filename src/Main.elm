module Main exposing (init, main, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.ListGroup as ListGroup
import Browser
import GroupsRender exposing (currentGroupControl, historyRender, marketGroupsRender)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Json exposing (..)
import MarketDecode
    exposing
        ( childGroups
        , decodeMarketGroups
        , getCurrentActive
        , getGroup
        , getGroupPatch
        , getRootGroups
        , getType
        , groupsIds
        , isHaveTypes
        , typeDecoder
        , typeListDecoder
        )
import Model exposing (..)
import Task exposing (Task)



---- MODEL ----


init : Value -> ( Model, Cmd Msg )
init marketGroups =
    let
        decoded =
            decodeMarketGroups marketGroups

        rootGroups =
            getRootGroups decoded
    in
    ( { marketGroups = decoded
      , marketTypes = Nothing
      , currentList = EntityListGroups rootGroups
      , navigation = Nothing
      , currentActive = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { marketGroups, marketTypes } =
            model

        getCurrentChildGroups =
            childGroups marketGroups

        currentRootGroups =
            getRootGroups marketGroups

        getCurrentGroup =
            getCurrentActive marketGroups

        getCurrentGroupPatch =
            getGroupPatch marketGroups

        newModel =
            case msg of
                SelectGroup id ->
                    case id of
                        Just i ->
                            ( { model
                                | currentList = getCurrentChildGroups i
                                , currentActive = getCurrentGroup i
                                , navigation = getCurrentGroupPatch i
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model
                                | currentList = EntityListGroups currentRootGroups
                                , currentActive = Nothing
                                , navigation = Nothing
                              }
                            , Cmd.none
                            )

                SelectType id ->
                    case isHaveTypes marketTypes id of
                        False ->
                            ( model
                            , Cmd.GetTypes id
                            )

                        True ->
                            ( { model }
                            , Cmd.none
                            )

                GetTypes id ->
                    case isHaveTypes marketTypes id of
                        False ->
                            ( model
                            , Task.attempt TypesReceived <| getTypes id
                            )

                        True ->
                            ( model
                            , Cmd.none
                            )

                TypesReceived types ->
                    case types of
                        Ok recived ->
                            let
                                _ =
                                    Debug.log "recived-types" recived
                            in
                            ( { model
                                | marketTypes = appendTypes model.marketTypes recived
                              }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model, Cmd.none )
    in
    newModel


appendTypes types newTypes =
    case types of
        Just oldTypes ->
            Just <| List.append oldTypes newTypes

        Nothing ->
            Just newTypes


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



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        { currentList, currentActive, navigation } =
            model
    in
    div []
        [ CDN.stylesheet
        , historyRender navigation
        , currentGroupControl currentActive
        , marketGroupsRender currentList
        ]



---- PROGRAM ----


main : Program Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
