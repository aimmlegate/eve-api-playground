module TypesRenders exposing (renderCurrentType, renderPrices)

import Html exposing (Html, a, div, h1, img, li, p, span, text, ul)
import Html.Attributes exposing (class)
import Model exposing (..)
import State exposing (..)


renderCurrentType : Model -> Html Msg
renderCurrentType { selectedType } =
    case selectedType of
        Just t ->
            div [ class "jumbotron" ]
                [ p [ class "display-4" ] [ text t.name ], p [] [ text t.description ] ]

        Nothing ->
            div [ class "jumbotron" ] []


renderPrices : Model -> Html Msg
renderPrices model =
    let
        { selectedType } =
            model
    in
    case selectedType of
        Just { type_id } ->
            let
                typePrice =
                    State.getPrice model type_id
            in
            case typePrice of
                Just price ->
                    renderPrice price

                Nothing ->
                    div [] []

        Nothing ->
            div [] []


renderPrice : Prices -> Html Msg
renderPrice { adjusted_price, average_price } =
    case ( adjusted_price, average_price ) of
        ( Just adj, Just avp ) ->
            div []
                [ p [ class "display-5" ] [ text <| "Adjusted Price: " ++ String.fromFloat adj ++ " ISK" ]
                , p [ class "display-5" ] [ text <| "Average Price: " ++ String.fromFloat avp ++ " ISK" ]
                ]

        ( Nothing, Just avp ) ->
            div []
                [ p [ class "display-5" ] [ text <| "Average Price: " ++ String.fromFloat avp ++ " ISK" ]
                ]

        ( Just adj, Nothing ) ->
            div []
                [ p [ class "display-5" ] [ text <| "Adjusted Price: " ++ String.fromFloat adj ++ " ISK" ]
                ]

        ( Nothing, Nothing ) ->
            div [] []
