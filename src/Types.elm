module Types exposing (..)

import Array exposing (Array)


type alias Coordinates =
    { row : Int
    , col : Int
    }


type ResourceKind
    = None -- TODO: change this to NoResource
    | Water
    | RawMetals
    | MetalAlloys
    | Silicon
    | DarkMatter
    | ExoticMinerals


resourceKindToName : ResourceKind -> String
resourceKindToName rk =
    case rk of
        None ->
            "Nothing"

        Water ->
            "Water"

        RawMetals ->
            "Raw Metals"

        MetalAlloys ->
            "Metal Alloys"

        Silicon ->
            "Silicon"

        DarkMatter ->
            "Dark Matter"

        ExoticMinerals ->
            "Exotic Minerals"


resourceKindToSymbol : ResourceKind -> String
resourceKindToSymbol rk =
    case rk of
        None ->
            "X"

        Water ->
            "W"

        RawMetals ->
            "R"

        MetalAlloys ->
            "M"

        Silicon ->
            "S"

        DarkMatter ->
            "D"

        ExoticMinerals ->
            "E"


intToResourceKind : Int -> ResourceKind
intToResourceKind i =
    case i of
        2 ->
            Water

        3 ->
            RawMetals

        4 ->
            MetalAlloys

        5 ->
            Silicon

        6 ->
            DarkMatter

        7 ->
            ExoticMinerals

        _ ->
            None


type alias ResourceData =
    { kind : ResourceKind
    , count : Int
    }


type Resource
    = Discovered ResourceData
    | Undiscovered


type SectorKind
    = DeepSpace
    | ColonizedSystem
    | UncolonizedSystem
    | Nebula
    | EnemySpace


sectorKindToName : SectorKind -> String
sectorKindToName sk =
    case sk of
        DeepSpace ->
            "Deep Space"

        ColonizedSystem ->
            "Colonized Star System"

        UncolonizedSystem ->
            "Uncolonized Star System"

        Nebula ->
            "Nebula"

        EnemySpace ->
            "Enemy Space"


sectorKindToSymbol : SectorKind -> String
sectorKindToSymbol sk =
    case sk of
        DeepSpace ->
            "D"

        ColonizedSystem ->
            "C"

        UncolonizedSystem ->
            "U"

        Nebula ->
            "N"

        EnemySpace ->
            "E"


intToSectorKind : Int -> SectorKind
intToSectorKind i =
    case i of
        2 ->
            ColonizedSystem

        3 ->
            UncolonizedSystem

        4 ->
            Nebula

        5 ->
            EnemySpace

        _ ->
            DeepSpace


type alias SectorData =
    { kind : SectorKind
    , resource : Resource
    }


type Sector
    = Mapped SectorData
    | Unmapped


maxSectorRow : Int
maxSectorRow =
    12


maxSectorCol : Int
maxSectorCol =
    12


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
    = Move Int -- Should the action get passed the data or should it be pulled from the model?
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


type alias Model =
    { sectors : Array (Array Sector)
    , upgradeProgress : UpgradeProgress
    , damage : Int
    , turnState : Maybe TurnState
    , location : Maybe Coordinates
    , hoveredAction : Maybe Action
    }


type Msg
    = SectorClicked Coordinates
    | RollDice
    | Rolled RollResult
    | HoveredAction Action
    | UnhoveredAction
    | SelectedAction Action
