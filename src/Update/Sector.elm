module Update.Sector exposing (..)

import Array exposing (Array)
import Types exposing (..)
import Types.Sector exposing (..)
import Types.Resource exposing (Resource(..))


arrayUpdate : Int -> (a -> a) -> Array a -> Array a
arrayUpdate i updateFn arr =
    case Array.get i arr of
        Nothing ->
            arr

        Just el ->
            Array.set i (updateFn el) arr


mapSector : TurnState -> Sector -> Sector
mapSector t sector =
    case sector of
        -- This should be unreachable due to validMapSector
        Mapped s ->
            Mapped s

        Unmapped ->
            Mapped
                { kind = intToSectorKind t.roll.d6
                , resource = Undiscovered
                }


getSector : Model -> Coordinates -> Maybe Sector
getSector model coords =
    case Array.get coords.row model.sectors of
        Nothing ->
            Nothing

        Just row ->
            Array.get coords.col row


updateSector : (Sector -> Sector) -> Array (Array Sector) -> Coordinates -> Array (Array Sector)
updateSector updateFn arr coords =
    arrayUpdate
        coords.row
        (\row -> arrayUpdate coords.col updateFn row)
        arr
