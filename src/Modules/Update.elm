module Update exposing (selectGroup, typesReceived)

import ESI exposing (..)
import Model exposing (..)
import State exposing (..)
import Task exposing (Task)


selectGroup : Model -> Maybe Int -> ( Model, Cmd Msg )
selectGroup model id =
    case ( id, isTerminalGroup model id ) of
        ( Nothing, _ ) ->
            ( { model
                | currentList = State.selectRoot model
                , currentActive = Nothing
                , navigation = Nothing
              }
            , Cmd.none
            )

        ( Just i, False ) ->
            ( { model
                | currentList = State.selectGroupsList model i
                , currentActive = State.selectGroup model i
                , navigation = State.buildNavigationList model i
              }
            , Cmd.none
            )

        ( Just i, True ) ->
            let
                selectedTypes =
                    State.selectTypesList model i
            in
            case selectedTypes of
                Just types ->
                    ( { model
                        | currentList = State.selectTypesList model i
                        , currentActive = State.selectGroup model i
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
