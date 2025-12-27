module Update.Resource exposing (..)

import Array exposing (Array)
import Types exposing (..)
import Types.Resource exposing (..)
import Types.Sector exposing (..)


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
