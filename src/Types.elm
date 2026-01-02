module Types exposing (..)

import Array exposing (Array)


maxSectorRow : Int
maxSectorRow =
    12


maxSectorCol : Int
maxSectorCol =
    12


numResourcesToUpgrade : Int
numResourcesToUpgrade =
    20


maxTurns : Int
maxTurns =
    30


type SectorKind
    = DeepSpace
    | ColonizedSystem
    | UncolonizedSystem
    | Nebula
    | EnemySpace


type alias SectorData =
    { kind : SectorKind
    , resource : Resource
    }


type Sector
    = Mapped SectorData
    | Unmapped


type ResourceKind
    = None -- TODO: change this to NoResource
    | Water
    | RawMetals
    | MetalAlloys
    | Silicon
    | DarkMatter
    | ExoticMinerals


type alias ResourceData =
    { kind : ResourceKind
    , count : Int
    }


type Resource
    = Discovered ResourceData
    | Undiscovered


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


type Upgrade
    = BlinkDrive
    | TerraformingTech
    | ShipRepairs
    | ScannerTech


type alias UpgradeProgress =
    { blinkDrive : Int
    , terraformingTech : Int
    , shipRepairs : Int
    , scannerTech : Int
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
    , turnNumber : Int
    }


type Msg
    = SectorClicked Coordinates
    | RollDice
    | Rolled RollResult
    | HoveredAction Action
    | UnhoveredAction
    | SelectedAction Action
    | PirateEncounter ( List Coordinates, List Coordinates )
    | ResourceCollected
        { data : ResourceData
        , location : Coordinates
        , applyTo : Upgrade
        }
