module Update.Resource exposing (..)

import Array exposing (Array)
import Data.Resource exposing (..)
import Data.Sector exposing (..)
import Types exposing (..)


resourceScan : TurnState -> Sector -> Sector
resourceScan t sector =
    case sector of
        -- This should be unreachable due to validResourceScan
        Unmapped ->
            Unmapped

        Mapped s ->
            case s.resource of
                -- This should be unreachable due to validResourceScan
                Discovered _ ->
                    Mapped s

                Undiscovered ->
                    Mapped
                        { s
                            | resource =
                                Discovered
                                    { kind = intToResourceKind t.roll.d8
                                    , count = t.roll.d12
                                    }
                        }


resourceMap : (Int -> Int -> ResourceData -> ResourceData) -> Array (Array Sector) -> Array (Array Sector)
resourceMap fn sectors =
    Array.indexedMap
        (\rowNum row ->
            Array.indexedMap
                (\colNum sector ->
                    case sector of
                        Unmapped ->
                            sector

                        Mapped s ->
                            Mapped <|
                                case s.resource of
                                    Undiscovered ->
                                        s

                                    Discovered r ->
                                        { s | resource = Discovered <| fn rowNum colNum r }
                )
                row
        )
        sectors


resourceUpdate : (ResourceData -> ResourceData) -> Coordinates -> Array (Array Sector) -> Array (Array Sector)
resourceUpdate fn coords sectors =
    case Array.get coords.row sectors of
        Nothing ->
            sectors

        Just row ->
            case Array.get coords.col row of
                Nothing ->
                    sectors

                Just sector ->
                    case sector of
                        Unmapped ->
                            sectors

                        Mapped s ->
                            case s.resource of
                                Undiscovered ->
                                    sectors

                                Discovered r ->
                                    Array.set coords.row
                                        (Array.set coords.col (Mapped { s | resource = Discovered (fn r) }) row)
                                        sectors
