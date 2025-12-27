module Rules exposing (..)

import Types exposing (..)


canScan : List Effect -> Bool
canScan activeEffects =
    not <| List.member (ScanningImpaired { failing = True }) activeEffects


gameDistance : Coordinates -> Coordinates -> Int
gameDistance a b =
    abs (a.row - b.row)
        + abs (a.col - b.col)


movementDistanceModifier : List Effect -> Int -> Int
movementDistanceModifier activeEffects distance =
    case List.member MovementImpaired activeEffects of
        True ->
            distance // 2

        False ->
            distance


scanningDistanceModifier : List Effect -> Int -> Int
scanningDistanceModifier activeEffects distance =
    case List.member (ScanningImpaired { failing = False }) activeEffects of
        True ->
            distance + 2

        False ->
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
