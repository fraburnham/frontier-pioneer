module Rules exposing (..)

import Data.Resource exposing (..)
import Data.Sector exposing (..)
import Types exposing (..)


canScan : List Effect -> Bool
canScan effects =
    not <| List.member (ScanningImpaired { failing = True }) effects


gameDistance : Coordinates -> Coordinates -> Int
gameDistance a b =
    abs (a.row - b.row)
        + abs (a.col - b.col)


movementImpaired : List Effect -> (Int -> Int)
movementImpaired effects =
    case List.member MovementImpaired effects of
        False ->
            identity

        True ->
            \d -> d // 2


movementImproved : List Effect -> (Int -> Int)
movementImproved effects =
    case List.member MovementImproved effects of
        False ->
            identity

        True ->
            \d -> d * 2


movementDistanceModifier : List Effect -> Int -> Int
movementDistanceModifier effects distance =
    -- TODO: rename, this is modifiedMovementDistance really
    movementImproved effects
        >> movementImpaired effects
    <|
        distance


scanningDistanceModifier : List Effect -> Int -> Int
scanningDistanceModifier effects distance =
    let
        getDistanceModifierFn =
            \eff ->
                case List.member eff effects of
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
validScanCommon effects range sector curLocation sectorLocation =
    canScan effects
        && ((gameDistance curLocation sectorLocation |> scanningDistanceModifier effects) <= range)


validMapSector : List Effect -> Int -> Sector -> Coordinates -> Coordinates -> Bool
validMapSector effects range sector curLocation sectorLocation =
    validScanCommon effects range sector curLocation sectorLocation
        && (case sector of
                Mapped _ ->
                    False

                Unmapped ->
                    True
           )


validResourceScan : TurnState -> List Effect -> Int -> Sector -> Coordinates -> Coordinates -> Bool
validResourceScan turnState effects range sector curLocation sectorLocation =
    -- TODO: Break the fns out and use an every of some kind
    validScanCommon effects range sector curLocation sectorLocation
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
                                        && (case Debug.log "Would get" <| intToResourceKind turnState.roll.d8 of
                                                Water ->
                                                    case s.kind of
                                                        DeepSpace ->
                                                            False

                                                        _ ->
                                                            True

                                                DarkMatter ->
                                                    case s.kind of
                                                        ColonizedSystem ->
                                                            False

                                                        _ ->
                                                            True

                                                ExoticMinerals ->
                                                    case s.kind of
                                                        ColonizedSystem ->
                                                            False

                                                        _ ->
                                                            True

                                                _ ->
                                                    True
                                           )
                           )
           )
