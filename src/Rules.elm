module Rules exposing (..)

import Data.Resource exposing (..)
import Data.Sector exposing (..)
import Types exposing (..)


canScan : List Effect -> Bool
canScan activeEffects =
    not <| List.member (ScanningImpaired { failing = True }) activeEffects


gameDistance : Coordinates -> Coordinates -> Int
gameDistance a b =
    abs (a.row - b.row)
        + abs (a.col - b.col)


movementImpaired : List Effect -> (Int -> Int)
movementImpaired activeEffects =
    case List.member MovementImpaired activeEffects of
        False ->
            identity

        True ->
            \d -> d // 2


movementImproved : List Effect -> (Int -> Int)
movementImproved activeEffects =
    case List.member MovementImproved activeEffects of
        False ->
            identity

        True ->
            \d -> d * 2


movementDistanceModifier : List Effect -> Int -> Int
movementDistanceModifier activeEffects distance =
    -- TODO: rename, this is modifiedMovementDistance really
    movementImproved activeEffects
        >> movementImpaired activeEffects
    <|
        distance


scanningDistanceModifier : List Effect -> Int -> Int
scanningDistanceModifier activeEffects distance =
    let
        getDistanceModifierFn =
            \eff ->
                case List.member eff activeEffects of
                    False ->
                        identity

                    True ->
                        case eff of
                            ScanningImpaired details ->
                                case details.failing of
                                    False ->
                                        \d -> d + 2

                                    True ->
                                        \d -> 0

                            _ ->
                                identity
    in
    List.foldl
        (\eff fn ->
            fn >> getDistanceModifierFn eff
        )
        identity
        [ ScanningImpaired { failing = False }, ScanningImpaired { failing = True } ]
    <|
        distance


validMove : Int -> Coordinates -> Coordinates -> Bool
validMove movesLeft curLocation newLocation =
    (movesLeft > 0) && (gameDistance curLocation newLocation == 1)


validMoveHover : Int -> Coordinates -> Coordinates -> Bool
validMoveHover movesLeft curLocation newLocation =
    (movesLeft > 0) && (gameDistance curLocation newLocation <= movesLeft)


validScanCommon : List Effect -> Int -> Sector -> Coordinates -> Coordinates -> Bool
validScanCommon activeEffects range sector curLocation sectorLocation =
    canScan activeEffects
        && ((gameDistance curLocation sectorLocation |> scanningDistanceModifier activeEffects) <= range)


validMapSector : List Effect -> Int -> Sector -> Coordinates -> Coordinates -> Bool
validMapSector activeEffects range sector curLocation sectorLocation =
    validScanCommon activeEffects range sector curLocation sectorLocation
        && (case sector of
                Mapped _ ->
                    False

                Unmapped ->
                    True
           )


validResourceScan : List Effect -> Int -> Sector -> Coordinates -> Coordinates -> Bool
validResourceScan activeEffects range sector curLocation sectorLocation =
    validScanCommon activeEffects range sector curLocation sectorLocation
        && (case sector of
                Unmapped ->
                    False

                Mapped s ->
                    True
                        && (case s.resource of
                                Discovered _ ->
                                    False

                                Undiscovered ->
                                    True
                           )
           )
