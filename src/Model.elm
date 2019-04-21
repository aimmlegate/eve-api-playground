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
    Int


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
    , marketTypes : MarketTypes
    , currentList : EntityList
    , currentActive : Maybe Entity
    , navigation : Maybe (List Entity)
    }


type Msg
    = SelectGroup (Maybe Int)
    | GetTypes Int
    | TypesReceived (Result Http.Error MarketTypes)
