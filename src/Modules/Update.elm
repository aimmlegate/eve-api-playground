module Update exposing (selectGroup)

import Model exposing (..)
import Task exposing (Task)


selectGroup : Model -> Int -> ( Model, Cmd Msg )
selectGroup model id =
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
