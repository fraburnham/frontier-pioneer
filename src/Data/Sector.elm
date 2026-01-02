module Data.Sector exposing (..)

import Array exposing (Array)
import Types exposing (..)


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


getCurrentSector : Model -> Maybe SectorData
getCurrentSector model =
    case model.location of
        Nothing ->
            Nothing

        Just l ->
            case Array.get l.row model.sectors of
                Nothing ->
                    Nothing

                Just row ->
                    case Array.get l.col row of
                        Nothing ->
                            Nothing

                        Just sec ->
                            case sec of
                                Unmapped ->
                                    Nothing

                                Mapped s ->
                                    Just s


sectorMap : (Coordinates -> Sector -> a) -> Array (Array Sector) -> Array (Array a)
sectorMap fn sectors =
    Array.indexedMap
        (\rowNum row ->
            Array.indexedMap
                (\colNum sector ->
                    fn { row = rowNum, col = colNum } sector
                )
                row
        )
        sectors


sectorMapWhenMapped : (Coordinates -> SectorData -> SectorData) -> Array (Array Sector) -> Array (Array Sector)
sectorMapWhenMapped fn sectors =
    sectorMap
        (\coords sector ->
            case sector of
                Unmapped ->
                    sector

                Mapped s ->
                    Mapped <|
                        fn coords s
        )
        sectors
