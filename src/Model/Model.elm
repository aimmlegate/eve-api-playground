module Model exposing
    ( Entity(..)
    , EntityList(..)
    , Group
    , MarketGroups
    , MarketTypes
    , Model
    , Msg(..)
    , Type
    )

import Http


type alias Group =
    { marketGroupID : Int
    , parentGroupID : Maybe Int
    , marketGroupName : String
    , description : String
    , iconID : Maybe Int
    , hasTypes : Int
    }


type alias Type =
    { description : String
    , name : String
    , market_group_id : Int
    , group_id : Int
    , type_id : Int
    }


type Entity
    = EntityGroup Group
    | EntityType Type


type EntityList
    = EntityListGroups MarketGroups
    | EntityListTypes MarketTypes


type alias MarketGroups =
    List Group


type alias MarketTypes =
    List Type


type alias Model =
    { marketGroups : MarketGroups
    , marketTypes : Maybe MarketTypes
    , currentList : Maybe EntityList
    , currentActive : Maybe Entity
    , selectedType : Maybe Type
    , navigation : Maybe (List Entity)
    }


type Msg
    = SelectGroup (Maybe Int)
    | TypesReceived (Result Http.Error MarketTypes)
    | SelectType Int
