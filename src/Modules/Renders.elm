module Renders exposing (currentGroupControl, historyRender, marketGroupsRender)

import Bootstrap.Breadcrumb as Breadcrumb
import Bootstrap.Button as Button
import Bootstrap.ListGroup as ListGroup
import Html exposing (Html, a, div, h1, img, text)
import Html.Events exposing (onClick)
import Model exposing (..)


marketGroupRender { marketGroupID, marketGroupName, hasTypes } =
    let
        handler =
            case hasTypes of
                0 ->
                    SelectGroup <| Just marketGroupID

                _ ->
                    -- GetTypes marketGroupID
                    -- SelectType marketGroupID
                    SelectGroup <| Just marketGroupID
    in
    ListGroup.button
        [ ListGroup.attrs [ onClick handler ] ]
        [ text marketGroupName ]


marketTypeRender { name } =
    ListGroup.button
        [ ListGroup.attrs [] ]
        [ text name ]


currentGroupControl : Maybe Entity -> Html Msg
currentGroupControl currentActive =
    case currentActive of
        Just entity ->
            case entity of
                EntityGroup { marketGroupName, parentGroupID } ->
                    div []
                        [ Button.button
                            [ Button.primary
                            , Button.attrs
                                [ onClick <| SelectGroup parentGroupID ]
                            ]
                            [ text "Back" ]
                        , h1 [] [ text marketGroupName ]
                        ]

                EntityType { name } ->
                    div []
                        [ Button.button
                            [ Button.primary
                            ]
                            [ text "Back" ]
                        , h1 [] [ text name ]
                        ]

        Nothing ->
            h1 [] [ text "Root" ]


marketGroupsRender marketGroups =
    case marketGroups of
        Just (EntityListGroups groups) ->
            ListGroup.custom <| List.map marketGroupRender groups

        Just (EntityListTypes types) ->
            ListGroup.custom <| List.map marketTypeRender types

        Nothing ->
            div [] [ text "loading" ]


historyItemRender historyNode =
    case historyNode of
        EntityGroup { marketGroupID, marketGroupName } ->
            Breadcrumb.item []
                [ a [ onClick <| SelectGroup <| Just marketGroupID ]
                    [ text marketGroupName ]
                ]

        EntityType { name } ->
            Breadcrumb.item []
                [ a []
                    [ text name ]
                ]


historyItemRenderRoot =
    Breadcrumb.item []
        [ a [ onClick <| SelectGroup Nothing ]
            [ text "All" ]
        ]


historyRender history =
    case history of
        Just hist ->
            Breadcrumb.container <|
                List.append [ historyItemRenderRoot ] <|
                    List.map historyItemRender hist

        Nothing ->
            Breadcrumb.container [ historyItemRenderRoot ]
