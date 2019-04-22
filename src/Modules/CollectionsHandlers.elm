module CollectionsHandlers exposing
    ( childGroups
    , getCurrentActive
    , getGroup
    , getGroupPatch
    , getRootGroups
    , getType
    , getTypes
    , groupsIds
    , isHaveTypesInState
    , isWithTypes
    , selectTypes
    )

import Model exposing (..)


isRootGroup { parentGroupID } =
    case parentGroupID of
        Nothing ->
            True

        _ ->
            False


isHaveTypesInState marketTypes id =
    case marketTypes of
        Just types ->
            List.any (\{ market_group_id } -> market_group_id == id) types

        Nothing ->
            False


isWithTypes marketGroup =
    case marketGroup of
        Just (EntityGroup x) ->
            x.hasTypes == 1

        _ ->
            False


getRootGroups : MarketGroups -> MarketGroups
getRootGroups marketGroups =
    List.filter isRootGroup marketGroups


getGroup : MarketGroups -> Int -> Maybe Group
getGroup marketGroups id =
    List.head <| List.filter (\{ marketGroupID } -> marketGroupID == id) marketGroups


getType : MarketTypes -> Int -> Maybe Type
getType marketTypes id =
    List.head <| List.filter (\{ type_id } -> type_id == id) marketTypes


getTypes marketTypes id =
    List.filter (\{ group_id } -> group_id == id) marketTypes


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


selectTypes : MarketTypes -> Int -> MarketTypes
selectTypes marketTypes marketGroupID =
    List.filter
        (\{ market_group_id } -> market_group_id == marketGroupID)
        marketTypes


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
