module Renders exposing (currentGroupControl, historyRender, marketGroupsRender, marketTreeRender)

import Bootstrap.Breadcrumb as Breadcrumb
import Bootstrap.Button as Button
import Bootstrap.ListGroup as ListGroup
import Html exposing (Html, a, div, h1, img, li, p, span, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Model exposing (..)
import State exposing (getEntityMarketId, getRootGroups, isRootGroup, isTerminalGroup, selectEntityChild, selectRoot)


marketGroupRender group =
    let
        { marketGroupID, marketGroupName, hasTypes } =
            group
    in
    case isRootGroup group of
        True ->
            li []
                [ span
                    [ class "font-weight-bold market-li text-uppercase text-muted", onClick <| SelectGroup <| Just marketGroupID ]
                    [ text marketGroupName ]
                ]

        False ->
            li []
                [ span
                    [ class "font-weight-normal market-li ", onClick <| SelectGroup <| Just marketGroupID ]
                    [ text marketGroupName ]
                ]


marketTypeRender { name } =
    li [ class "market-li-types" ]
        [ span
            [ class "font-weight-normal market-li text-muted" ]
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
    div [ class "mt-3" ]
        [ case history of
            Just hist ->
                Breadcrumb.container <|
                    List.append [ historyItemRenderRoot ] <|
                        List.map historyItemRender hist

            Nothing ->
                Breadcrumb.container [ historyItemRenderRoot ]
        ]


marketTreeRender : Model -> Html Msg
marketTreeRender model =
    let
        { marketGroups, marketTypes, currentList, navigation, currentActive } =
            model

        rootGroups =
            selectRoot model

        isCurrentHeveTypes =
            case currentActive of
                Just (EntityGroup { hasTypes }) ->
                    1 == hasTypes

                _ ->
                    False

        submenuRender entityList nav =
            case ( entityList, nav, isCurrentHeveTypes ) of
                ( Nothing, _, True ) ->
                    case currentList of
                        Just (EntityListTypes types) ->
                            ul [ class "market-ul" ] <|
                                List.map marketTypeRender
                                    types

                        Nothing ->
                            div [] [ text "loading" ]

                        _ ->
                            div [] [ text "error" ]

                ( Just (EntityListGroups groups), Just (h :: hs), _ ) ->
                    ul [ class "market-ul" ] <|
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

                ( Just (EntityListGroups groups), Just [], _ ) ->
                    ul [ class "market-ul" ] <|
                        List.map marketGroupRender
                            groups

                _ ->
                    ul [ class "market-ul" ] <|
                        List.map marketGroupRender <|
                            getRootGroups marketGroups
    in
    div [ class "vh-100 market-list-cont pt-3" ]
        [ submenuRender rootGroups navigation ]
