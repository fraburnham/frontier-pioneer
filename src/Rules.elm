module Rules exposing (..)

import Data.Effect exposing (..)
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
    if List.member MovementImpaired effects then
        \d -> d // 2

    else
        identity


movementImproved : List Effect -> (Int -> Int)
movementImproved effects =
    if List.member MovementImproved effects then
        \d -> d * 2

    else
        identity


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
                if List.member eff effects then
                    case eff of
                        ScanningImpaired details ->
                            if details.failing then
                                \_ -> 0

                            else
                                \d -> d + 2

                        _ ->
                            identity

                else
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


validMoveModel : Model -> Coordinates -> Bool
validMoveModel model newLocation =
    case model.location of
        Nothing ->
            False

        Just l ->
            case model.turnState of
                Nothing ->
                    False

                Just t ->
                    validMoveHover t.roll.d4 l newLocation


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


validMapSectorModel : Model -> Coordinates -> Bool
validMapSectorModel model sectorLocation =
    case model.location of
        Nothing ->
            False

        Just l ->
            case model.turnState of
                Nothing ->
                    False

                Just t ->
                    case getSector model sectorLocation of
                        Nothing ->
                            False

                        Just s ->
                            validMapSector (activeEffects model) t.roll.d10 s l sectorLocation


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
                                        && (case intToResourceKind turnState.roll.d8 of
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


validResourceScanModel : Model -> Coordinates -> Bool
validResourceScanModel model sectorLocation =
    case model.location of
        Nothing ->
            False

        Just l ->
            case model.turnState of
                Nothing ->
                    False

                Just t ->
                    case getSector model sectorLocation of
                        Nothing ->
                            False

                        Just s ->
                            validResourceScan t (activeEffects model) t.roll.d10 s l sectorLocation
