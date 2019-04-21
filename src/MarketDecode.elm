module MarketDecode exposing
    ( childGroups
    , decodeMarketGroups
    , getCurrentActive
    , getGroup
    , getGroupPatch
    , getRootGroups
    , groupsIds
    , isHaveTypes
    , typeDecoder
    , typeListDecoder
    )

import Dict exposing (..)
import Json.Decode as Json exposing (..)
import Json.Encode
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


isRootGroup { parentGroupID } =
    case parentGroupID of
        Nothing ->
            True

        _ ->
            False


isHaveTypes marketTypes id =
    case marketTypes of
        Just types ->
            List.any (\{ market_group_id } -> market_group_id == id) types

        Nothing ->
            False


decodeMarketGroups : Value -> MarketGroups
decodeMarketGroups value =
    case Json.decodeValue (Json.list decodeMarketGroup) value of
        Ok list ->
            list

        _ ->
            []


getRootGroups : MarketGroups -> MarketGroups
getRootGroups marketGroups =
    List.filter isRootGroup marketGroups


getGroup : MarketGroups -> Int -> Maybe Group
getGroup marketGroups id =
    List.head <| List.filter (\{ marketGroupID } -> marketGroupID == id) marketGroups


getType : MarketTypes -> Int -> Maybe Type
getType marketTypes id =
    List.head <| List.filter (\{ type_id } -> type_id == id) marketTypes


getCurrentActive marketGroups id =
    let
        finded =
            getGroup marketGroups id
    in
    case finded of
        Nothing ->
            Nothing

        Just x ->
            Just <| EntityGroup x


childGroups : MarketGroups -> Int -> EntityList
childGroups marketGroups parentId =
    EntityListGroups <|
        List.filter
            (\{ parentGroupID } ->
                case parentGroupID of
                    Just id ->
                        parentId == id

                    Nothing ->
                        False
            )
            marketGroups


groupsIds marketGroups =
    List.map .marketGroupID marketGroups


getGroupPatch : MarketGroups -> Int -> Maybe (List Entity)
getGroupPatch marketGroups id =
    let
        lastSelectedGroup =
            getGroup marketGroups id

        helper acc en =
            case en of
                Just e ->
                    let
                        { parentGroupID } =
                            e
                    in
                    case parentGroupID of
                        Just i ->
                            helper (EntityGroup e :: acc) <| getGroup marketGroups i

                        Nothing ->
                            EntityGroup e :: acc

                Nothing ->
                    acc
    in
    case lastSelectedGroup of
        Just entity ->
            Just <| helper [] lastSelectedGroup

        Nothing ->
            Nothing
