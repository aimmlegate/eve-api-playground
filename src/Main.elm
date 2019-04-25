module Main exposing (init, main, update, view)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Navbar as Navbar
import Browser
import Decoders exposing (..)
import GroupsRenders exposing (historyRender, marketTreeRender)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Json exposing (..)
import Model exposing (..)
import State exposing (..)
import Update exposing (..)



---- MODEL ----


init : Value -> ( Model, Cmd Msg )
init marketGroups =
    let
        decoded =
            Decoders.decodeMarketGroups marketGroups

        rootGroups =
            State.getRootGroups decoded
    in
    ( { marketGroups = decoded
      , marketTypes = Nothing
      , currentList = Just <| EntityListGroups rootGroups
      , navigation = Nothing
      , selectedType = Nothing
      , currentActive = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectGroup id ->
            Update.selectGroup model id

        TypesReceived types ->
            Update.typesReceived model types

        SelectType id ->
            Update.selectType model id



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        { currentList, currentActive, navigation } =
            model
    in
    div []
        [ CDN.stylesheet
        , Grid.containerFluid []
            [ Grid.simpleRow
                [ Grid.col
                    [ Col.xl2, Col.lg3, Col.md4 ]
                    [ marketTreeRender model
                    ]
                , Grid.col
                    [ Col.xl10, Col.lg9, Col.md8 ]
                    [ historyRender navigation ]
                ]
            ]
        ]


main : Program Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
