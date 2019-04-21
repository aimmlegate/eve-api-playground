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
        , groupsIds
        )
import Model exposing (..)



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
      , marketTypes = [ 0 ]
      , currentList = EntityListGroups rootGroups
      , navigation = Nothing
      , currentActive = Nothing
      }
    , Cmd.none
    )


typeDecoder =
    field "types" (Json.list Json.int)



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { marketGroups } =
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

                GetTypes id ->
                    ( model
                    , Http.get
                        { url = "https://esi.evetech.net/latest/markets/groups/" ++ String.fromInt id
                        , expect = Http.expectJson TypesReceived typeDecoder
                        }
                    )

                TypesReceived result ->
                    case result of
                        Ok resp ->
                            let
                                _ =
                                    Debug.log "sss" result
                            in
                            ( model, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )
    in
    newModel



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
