module Types.Sector exposing (..)

import Types.Resource exposing (Resource)


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


maxSectorRow : Int
maxSectorRow =
    12


maxSectorCol : Int
maxSectorCol =
    12


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

        UncolonizedSystem ->
            "U"

        Nebula ->
            "N"

        ColonizedSystem ->
            "C"

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
