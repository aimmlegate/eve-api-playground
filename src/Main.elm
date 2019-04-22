module Main exposing (init, main, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.ListGroup as ListGroup
import Browser
import CollectionsHandlers exposing (..)
import DumpsDecoders exposing (decodeMarketGroups)
import EveApi exposing (getTypes)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Json exposing (..)
import Model exposing (..)
import Renders exposing (currentGroupControl, historyRender, marketGroupsRender)
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
      , currentList = Just <| EntityListGroups rootGroups
      , navigation = Nothing
      , currentActive = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


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
                            selectGroup model i

                        Nothing ->
                            ( { model
                                | currentList = Just <| EntityListGroups <| currentRootGroups
                                , currentActive = Nothing
                                , navigation = Nothing
                              }
                            , Cmd.none
                            )

                TypesReceived types ->
                    case types of
                        Ok recived ->
                            ( { model
                                | marketTypes = appendTypes model.marketTypes recived
                                , currentList = Just <| EntityListTypes recived
                              }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model, Cmd.none )
    in
    newModel


selectGroup : Model -> Int -> ( Model, Cmd Msg )
selectGroup model id =
    let
        { marketGroups, marketTypes } =
            model

        selectedGroup =
            getCurrentActive marketGroups id
    in
    case CollectionsHandlers.isWithTypes selectedGroup of
        False ->
            ( { model
                | currentList = Just <| childGroups marketGroups id
                , currentActive = selectedGroup
                , navigation = getGroupPatch marketGroups id
              }
            , Cmd.none
            )

        True ->
            if CollectionsHandlers.isHaveTypesInState marketTypes id then
                ( { model
                    | currentList = Just <| EntityListTypes <| CollectionsHandlers.getTypes (Maybe.withDefault [] marketTypes) id
                    , currentActive = selectedGroup
                    , navigation = getGroupPatch marketGroups id
                  }
                , Cmd.none
                )

            else
                ( { model
                    | currentList = Nothing
                    , currentActive = selectedGroup
                    , navigation = getGroupPatch marketGroups id
                  }
                , Task.attempt TypesReceived <| getTypes id
                )


appendTypes types newTypes =
    case types of
        Just oldTypes ->
            Just <| List.append oldTypes newTypes

        Nothing ->
            Just newTypes



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
