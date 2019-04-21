module GroupsRender exposing (currentGroupControl, historyRender, marketGroupsRender)

import Bootstrap.Breadcrumb as Breadcrumb
import Bootstrap.Button as Button
import Bootstrap.ListGroup as ListGroup
import Html exposing (Html, a, div, h1, img, text)
import Html.Events exposing (onClick)
import Model exposing (..)


marketGroupRender { marketGroupID, marketGroupName } =
    ListGroup.button
        [ ListGroup.attrs [ onClick <| SelectGroup <| Just marketGroupID ] ]
        [ text marketGroupName ]


marketTypeRender i =
    ListGroup.button
        [ ListGroup.attrs [] ]
        [ text <| String.fromInt i ]


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

                EntityType i ->
                    div []
                        [ Button.button
                            [ Button.primary
                            ]
                            [ text "Back" ]
                        , h1 [] [ text <| String.fromInt i ]
                        ]

        Nothing ->
            h1 [] [ text "Root" ]


marketGroupsRender marketGroups =
    case marketGroups of
        EntityListGroups groups ->
            ListGroup.custom <| List.map marketGroupRender groups

        EntityListTypes types ->
            ListGroup.custom <| List.map marketTypeRender types


historyItemRender historyNode =
    case historyNode of
        EntityGroup { marketGroupID, marketGroupName } ->
            Breadcrumb.item []
                [ a [ onClick <| SelectGroup <| Just marketGroupID ]
                    [ text marketGroupName ]
                ]

        EntityType i ->
            Breadcrumb.item []
                [ a []
                    [ text <| String.fromInt i ]
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
