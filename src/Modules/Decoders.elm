module Decoders exposing (decodeMarketGroup, decodeMarketGroups, priceDecoder, typeDecoder, typeListDecoder)

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


typeListDecoder =
    field "types" (Json.list Json.int)


typeDecoder =
    Json.map5 Type
        (field "description" Json.string)
        (field "name" Json.string)
        (field "market_group_id" Json.int)
        (field "group_id" Json.int)
        (field "type_id" Json.int)


priceDecoder : Json.Decoder (List Prices)
priceDecoder =
    Json.list
        (Json.map3 Prices
            (Json.maybe (field "adjusted_price" Json.float))
            (Json.maybe (field "average_price" Json.float))
            (field "type_id" Json.int)
        )


decodeMarketGroups : Value -> MarketGroups
decodeMarketGroups value =
    case Json.decodeValue (Json.list decodeMarketGroup) value of
        Ok list ->
            list

        _ ->
            []
