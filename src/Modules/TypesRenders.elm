module TypesRenders exposing (renderCurrentType)

import Html exposing (Html, a, div, h1, img, li, p, span, text, ul)
import Html.Attributes exposing (class)
import State exposing (..)


renderCurrentType { selectedType } =
    case selectedType of
        Just t ->
            div [ class "jumbotron" ]
                [ p [ class "display-4" ] [ text t.name ], p [] [ text t.description ] ]

        Nothing ->
            div [ class "jumbotron" ] []
