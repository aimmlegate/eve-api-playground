module Update exposing (selectGroup, typesReceived)

import ESI exposing (..)
import Http
import Model exposing (..)
import State exposing (..)
import Task exposing (Task)


isSameCurrent { currentActive } i =
    case ( currentActive, i ) of
        ( Just entity, Just id ) ->
            getEntityMarketId entity == id

        _ ->
            False


selectGroup : Model -> Maybe Int -> ( Model, Cmd Msg )
selectGroup model id =
    case ( id, isTerminalGroup model id, isSameCurrent model id ) of
        ( Nothing, _, _ ) ->
            ( { model
                | currentList = State.selectRoot model
                , selectedType = Nothing
                , navigation = Nothing
              }
            , Cmd.none
            )

        ( _, _, True ) ->
            let
                parentId =
                    getEntityMarketParentId model.currentActive
            in
            case parentId of
                Just i ->
                    ( { model
                        | currentList = State.selectGroupsList model i
                        , navigation = State.buildNavigationList model i
                      }
                    , Cmd.none
                    )

                Nothing ->
                    selectGroup model Nothing

        ( Just i, False, _ ) ->
            ( { model
                | currentList = State.selectGroupsList model i
                , navigation = State.buildNavigationList model i
              }
            , Cmd.none
            )

        ( Just i, True, _ ) ->
            let
                selectedTypes =
                    State.selectTypesListCurrent model i
            in
            case selectedTypes of
                Just types ->
                    ( { model
                        | currentList = selectedTypes
                        , navigation = State.buildNavigationList model i
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | currentList = Nothing
                        , currentActive = State.selectGroup model i
                        , navigation = State.buildNavigationList model i
                      }
                    , Task.attempt TypesReceived <| ESI.getTypes i
                    )


typesReceived : Model -> Result Http.Error MarketTypes -> ( Model, Cmd Msg )
typesReceived model types =
    case types of
        Ok recived ->
            ( { model
                | marketTypes = State.appendTypes model.marketTypes recived
                , currentList = Just <| EntityListTypes recived
              }
            , Cmd.none
            )

        Err _ ->
            ( model, Cmd.none )
