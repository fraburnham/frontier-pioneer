module Update exposing (update)

import Array exposing (Array)
import Random
import Random.Extra exposing (andMap)
import Random.List
import Rules exposing (..)
import Types exposing (..)


rollDice : Cmd Msg
rollDice =
    Random.generate Rolled
        (Random.constant
            (\d4 d6 d8 d10 d12 d20 ->
                { d4 = d4
                , d6 = d6
                , d8 = d8
                , d10 = d10
                , d12 = d12
                , d20 = d20
                }
            )
            |> andMap (Random.int 1 4)
            |> andMap (Random.int 1 6)
            |> andMap (Random.int 1 8)
            |> andMap (Random.int 0 9)
            |> andMap (Random.int 1 12)
            |> andMap (Random.int 1 20)
        )


removeHoveredAction : Model -> Model
removeHoveredAction model =
    { model | hoveredAction = Nothing }


getSector : Model -> Coordinates -> Maybe Sector
getSector model coords =
    case Array.get coords.row model.sectors of
        Nothing ->
            Nothing

        Just row ->
            Array.get coords.col row


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


arrayUpdate : Int -> (a -> a) -> Array a -> Array a
arrayUpdate i updateFn arr =
    case Array.get i arr of
        Nothing ->
            arr

        Just el ->
            Array.set i (updateFn el) arr


updateSector : (Sector -> Sector) -> Array (Array Sector) -> Coordinates -> Array (Array Sector)
updateSector updateFn arr coords =
    arrayUpdate
        coords.row
        (\row -> arrayUpdate coords.col updateFn row)
        arr


sectorClicked : Model -> Coordinates -> Model
sectorClicked model coords =
    let
        effects =
            activeEffects model
    in
    case model.location of
        Nothing ->
            { model | location = Just coords }

        Just l ->
            case model.turnState of
                Nothing ->
                    model

                Just t ->
                    case t.action of
                        NoAction ->
                            model

                        Move movesLeft ->
                            case validMove movesLeft l coords of
                                True ->
                                    { model
                                        | location = Just coords
                                        , turnState =
                                            case movesLeft of
                                                1 ->
                                                    Nothing

                                                _ ->
                                                    Just { t | action = Move (movesLeft - 1) }
                                    }

                                False ->
                                    model

                        MapSector ->
                            case getSector model coords of
                                Nothing ->
                                    model

                                Just s ->
                                    case validMapSector effects t.roll.d10 s l coords of
                                        True ->
                                            { model
                                                | sectors = updateSector (mapSector t) model.sectors coords
                                                , turnState = Nothing
                                            }

                                        _ ->
                                            model

                        ResourceScan ->
                            case getSector model coords of
                                Nothing ->
                                    model

                                Just s ->
                                    case validResourceScan effects t.roll.d10 s l coords of
                                        False ->
                                            model

                                        True ->
                                            { model
                                                | sectors = updateSector (resourceScan t) model.sectors coords
                                                , turnState = Nothing
                                            }

                        Anomaly ->
                            model


updateTurnStateAction : Action -> Model -> Model
updateTurnStateAction action model =
    case model.turnState of
        Nothing ->
            model

        Just t ->
            { model | turnState = Just { t | action = action } }


handleDamage : Model -> TurnState -> Model
handleDamage model t =
    { model | damage = model.damage + t.roll.d6 }


setTemporaryEffect : Model -> Effect -> Model
setTemporaryEffect model eff =
    { model | temporaryEffect = Just eff }


clearTemporaryEffect : Model -> Model
clearTemporaryEffect model =
    { model
        | temporaryEffect =
            case model.turnState of
                Nothing ->
                    Nothing

                Just t ->
                    case t.action of
                        Anomaly ->
                            model.temporaryEffect

                        _ ->
                            Nothing
    }


handleAnomaly : Model -> ( Model, Cmd Msg )
handleAnomaly model =
    -- TODO: gonna need tests on this. The anomaly is complex cabbage.
    case model.location of
        Nothing ->
            -- This should be unreachable because the location must be chosen to start the game
            ( model, Cmd.none )

        Just l ->
            case model.turnState of
                Nothing ->
                    ( model, Cmd.none )

                Just t ->
                    case t.action of
                        Anomaly ->
                            case t.roll.d8 of
                                1 ->
                                    -- Space Rift
                                    ( updateTurnStateAction (Move t.roll.d6) model, Cmd.none )

                                2 ->
                                    -- Energy Surge
                                    ( setTemporaryEffect model <| ScanningImpaired { failing = False }, Cmd.none )

                                3 ->
                                    -- Asteriod Shower
                                    ( handleDamage model t, Cmd.none )

                                4 ->
                                    -- Gravitational Distortion
                                    let
                                        range =
                                            max 1 <| t.roll.d4 // 2

                                        destroyResources =
                                            \row col sector ->
                                                case sector of
                                                    Unmapped ->
                                                        sector

                                                    Mapped s ->
                                                        Mapped <|
                                                            case gameDistance l { row = row, col = col } <= range of
                                                                False ->
                                                                    s

                                                                True ->
                                                                    case s.resource of
                                                                        Undiscovered ->
                                                                            s

                                                                        Discovered r ->
                                                                            { s | resource = Discovered { r | count = 0 } }
                                    in
                                    ( { model
                                        | sectors =
                                            Array.indexedMap (\rowNum row -> Array.indexedMap (destroyResources rowNum) row) model.sectors
                                      }
                                    , Cmd.none
                                    )

                                5 ->
                                    -- Temporal Distortion
                                    ( setTemporaryEffect model <| MovementImpaired, Cmd.none )

                                6 ->
                                    -- Alien Signal
                                    ( setTemporaryEffect model <| ScanningImpaired { failing = True }, Cmd.none )

                                7 ->
                                    -- Alien Encounter
                                    ( handleDamage model t, Cmd.none )

                                8 ->
                                    -- Pirates
                                    let
                                        minRow =
                                            max 0 <| l.row - t.roll.d4

                                        maxRow =
                                            min (maxSectorRow - 1) <| l.row + t.roll.d4

                                        minCol =
                                            max 0 <| l.col - t.roll.d4

                                        maxCol =
                                            min (maxSectorCol - 1) <| l.col + t.roll.d4

                                        coordsList =
                                            List.concatMap
                                                (\row ->
                                                    List.map
                                                        (\col ->
                                                            { col = col, row = row }
                                                        )
                                                    <|
                                                        List.range minCol maxCol
                                                )
                                            <|
                                                List.range minRow maxRow

                                        generator =
                                            Random.List.choices t.roll.d6 coordsList
                                    in
                                    ( model
                                    , Random.generate PirateEncounter generator
                                    )

                                _ ->
                                    -- This should be unreachable due to how the numbers are bound when generating
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SectorClicked c ->
            ( sectorClicked model c
            , Cmd.none
            )

        RollDice ->
            -- TODO: rename RollDice to StartTurn
            ( removeHoveredAction model |> clearTemporaryEffect
            , rollDice
            )

        Rolled result ->
            handleAnomaly
                { model
                    | turnState =
                        Just
                            { roll = result
                            , action =
                                case result.d20 of
                                    20 ->
                                        Anomaly

                                    _ ->
                                        NoAction
                            }
                }

        HoveredAction ha ->
            ( { model | hoveredAction = Just ha }
            , Cmd.none
            )

        UnhoveredAction ->
            ( removeHoveredAction model
            , Cmd.none
            )

        SelectedAction sa ->
            ( Debug.log "Action model post update" <| removeHoveredAction <| updateTurnStateAction sa model
            , Cmd.none
            )

        -- TODO: make a function sectorMap that takes the sectors and handles when to apply a fn that is (row -> col -> sector -> sector)
        PirateEncounter ( impactedCoordinates, _ ) ->
            ( { model
                | sectors =
                    Array.indexedMap
                        (\rowNum row ->
                            Array.indexedMap
                                (\colNum sector ->
                                    case List.member { row = rowNum, col = colNum } impactedCoordinates of
                                        False ->
                                            sector

                                        True ->
                                            case sector of
                                                Unmapped ->
                                                    sector

                                                Mapped s ->
                                                    Mapped <|
                                                        case s.resource of
                                                            Undiscovered ->
                                                                s

                                                            Discovered r ->
                                                                { s | resource = Discovered { r | count = 0 } }
                                )
                                row
                        )
                        model.sectors
              }
            , Cmd.none
            )
