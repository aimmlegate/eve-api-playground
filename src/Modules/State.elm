module State exposing
    ( appendTypes
    , buildNavigationList
    , getEntityMarketId
    , getEntityMarketParentId
    , getRootGroups
    , isTerminalGroup
    , selectEntityChild
    , selectGroup
    , selectGroupsList
    , selectRoot
    , selectType
    , selectTypesList
    )

import Model exposing (..)



---- EXPOSING ----
-- selectEntityChild : Model -> Maybe EntityList


selectEntityChild model entity =
    let
        { currentActive, marketGroups } =
            model
    in
    case entity of
        EntityGroup { marketGroupID } ->
            Just <|
                EntityListGroups <|
                    List.filter
                        (\group ->
                            marketGroupID == Maybe.withDefault -1 group.parentGroupID
                        )
                        marketGroups

        _ ->
            Nothing


getEntityMarketId : Entity -> Int
getEntityMarketId entity =
    case entity of
        EntityGroup { marketGroupID } ->
            marketGroupID

        EntityType { market_group_id } ->
            market_group_id


getEntityMarketParentId : Maybe Entity -> Maybe Int
getEntityMarketParentId entity =
    case entity of
        Just (EntityGroup { parentGroupID }) ->
            parentGroupID

        Just (EntityType { market_group_id }) ->
            Just market_group_id

        _ ->
            Nothing


selectGroupsList : Model -> Int -> Maybe EntityList
selectGroupsList { marketGroups } id =
    let
        selected =
            childGroups marketGroups id
    in
    case selected of
        [] ->
            Nothing

        v ->
            Just <| EntityListGroups v


selectGroup : Model -> Int -> Maybe Entity
selectGroup { marketGroups } id =
    let
        selected =
            getGroup marketGroups id
    in
    case selected of
        Just x ->
            Just <| EntityGroup x

        Nothing ->
            Nothing


selectTypesList : Model -> Int -> Maybe EntityList
selectTypesList { marketTypes } id =
    case marketTypes of
        Nothing ->
            Nothing

        Just types ->
            let
                selected =
                    getTypes types id
            in
            case selected of
                [] ->
                    Nothing

                v ->
                    Just <| EntityListTypes v


selectType : Model -> Int -> Maybe Entity
selectType { marketTypes } id =
    case marketTypes of
        Nothing ->
            Nothing

        Just types ->
            let
                selected =
                    getType types id
            in
            case selected of
                Just v ->
                    Just <| EntityType v

                Nothing ->
                    Nothing


buildNavigationList : Model -> Int -> Maybe (List Entity)
buildNavigationList { marketGroups } id =
    getGroupPatch marketGroups id


selectRoot : Model -> Maybe EntityList
selectRoot { marketGroups } =
    Just <| EntityListGroups <| getRootGroups marketGroups


isTerminalGroup : Model -> Maybe Int -> Bool
isTerminalGroup { marketGroups } id =
    case id of
        Nothing ->
            False

        Just i ->
            let
                selected =
                    getGroup marketGroups i
            in
            case selected of
                Just v ->
                    v.hasTypes == 1

                Nothing ->
                    False


appendTypes : Maybe MarketTypes -> MarketTypes -> Maybe MarketTypes
appendTypes types newTypes =
    case types of
        Just oldTypes ->
            Just <| List.append oldTypes newTypes

        Nothing ->
            Just newTypes



---- INTERNAL ----


isRootGroup : Group -> Bool
isRootGroup { parentGroupID } =
    case parentGroupID of
        Nothing ->
            True

        _ ->
            False


isHaveTypesInState : Maybe MarketTypes -> Int -> Bool
isHaveTypesInState marketTypes id =
    case marketTypes of
        Just types ->
            List.any (\{ market_group_id } -> market_group_id == id) types

        Nothing ->
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


getTypes : MarketTypes -> Int -> MarketTypes
getTypes marketTypes id =
    List.filter (\{ group_id } -> group_id == id) marketTypes


childGroups : MarketGroups -> Int -> MarketGroups
childGroups marketGroups parentId =
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
