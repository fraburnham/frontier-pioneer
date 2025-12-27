module Types exposing (..)

import Array exposing (Array)
import Types.Resource exposing (..)
import Types.Sector exposing (..)


type alias Coordinates =
    { row : Int
    , col : Int
    }


type alias RollResult =
    { d4 : Int
    , d6 : Int
    , d8 : Int
    , d10 : Int
    , d12 : Int
    , d20 : Int
    }


type Die
    = D4
    | D6
    | D8
    | D10
    | D12
    | D20


type Action
    = Move Int
    | MapSector
    | ResourceScan
    | Anomaly
    | NoAction


type alias TurnState =
    { roll : RollResult
    , action : Action
    }


type alias UpgradeProgress =
    { blinkDrive : Int
    , terraformingTechnology : Int
    , shipImprovements : Int
    , scannerTechnology : Int
    }


type Effect
    = ScanningImpaired { failing : Bool }
    | MovementImpaired
    | ShipImproved
    | ResourceDiscoveryImproved
    | ScanningImproved
    | MovementImproved


type alias Model =
    { sectors : Array (Array Sector)
    , upgradeProgress : UpgradeProgress
    , damage : Int
    , turnState : Maybe TurnState
    , location : Maybe Coordinates
    , hoveredAction : Maybe Action
    , effects : List Effect
    , temporaryEffect : Maybe Effect
    }


type Msg
    = SectorClicked Coordinates
    | RollDice
    | Rolled RollResult
    | HoveredAction Action
    | UnhoveredAction
    | SelectedAction Action
    | PirateEncounter ( List Coordinates, List Coordinates )


activeEffects : Model -> List Effect
activeEffects model =
    case model.temporaryEffect of
        Nothing ->
            model.effects

        Just e ->
            e :: model.effects
