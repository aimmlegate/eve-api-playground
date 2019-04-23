module Renders exposing (currentGroupControl, experimentalRender, historyRender, marketGroupsRender)

import Bootstrap.Breadcrumb as Breadcrumb
import Bootstrap.Button as Button
import Bootstrap.ListGroup as ListGroup
import Html exposing (Html, a, div, h1, img, li, p, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Model exposing (..)
import State exposing (getEntityMarketId, getRootGroups, isTerminalGroup, selectEntityChild, selectRoot)


marketGroupRender { marketGroupID, marketGroupName, hasTypes } =
    let
        handler =
            case hasTypes of
                0 ->
                    SelectGroup <| Just marketGroupID

                _ ->
                    SelectGroup <| Just marketGroupID
    in
    li []
        [ p
            [ class "font-weight-bold", onClick handler ]
            [ text marketGroupName ]
        ]


marketTypeRender { name } =
    li []
        [ p
            []
            [ text name ]
        ]


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
            ul [] <| List.map marketGroupRender groups

        Just (EntityListTypes types) ->
            ul [] <| List.map marketTypeRender types

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


experimentalRender : Model -> Html Msg
experimentalRender model =
    let
        { marketGroups, marketTypes, currentList, navigation } =
            model

        rootGroups =
            selectRoot model

        submenuRender entityList nav =
            case ( entityList, nav ) of
                ( Just (EntityListTypes types), Just [] ) ->
                    ul [] <|
                        List.map
                            marketTypeRender
                            types

                ( Just (EntityListGroups groups), Just (h :: hs) ) ->
                    ul [] <|
                        List.map
                            (\group ->
                                case group.marketGroupID == getEntityMarketId h of
                                    False ->
                                        marketGroupRender group

                                    True ->
                                        div []
                                            [ marketGroupRender group
                                            , li
                                                []
                                                [ submenuRender (selectEntityChild model h) (Just hs) ]
                                            ]
                            )
                            groups

                ( Just (EntityListGroups groups), Just [] ) ->
                    ul [] <|
                        List.map marketGroupRender
                            groups

                ( _, Just ((EntityGroup h) :: _) ) ->
                    if isTerminalGroup model (Just h.marketGroupID) then
                        submenuRender currentList nav

                    else
                        div [] [ text "loading" ]

                ( Nothing, _ ) ->
                    div [] [ text "loading" ]

                _ ->
                    ul [] <|
                        List.map marketGroupRender <|
                            getRootGroups marketGroups
    in
    submenuRender rootGroups navigation
