module DumpsDecoders exposing (decodeMarketGroups)

import Json.Decode as Json exposing (..)
import Model exposing (..)


decodeMarketGroup : Json.Decoder Group
decodeMarketGroup =
    Json.map6 Group
        (field "marketGroupID" Json.int)
        (field "parentGroupID" <| Json.maybe Json.int)
        (field "marketGroupName" Json.string)
        (field "description" Json.string)
        (field "iconID" <| Json.maybe Json.int)
        (field "hasTypes" Json.int)


decodeMarketGroups : Value -> MarketGroups
decodeMarketGroups value =
    case Json.decodeValue (Json.list decodeMarketGroup) value of
        Ok list ->
            list

        _ ->
            []
