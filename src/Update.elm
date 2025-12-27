module Update exposing (update)

import Array exposing (Array)
import Random
import Rules exposing (..)
import Types exposing (..)


rollDice : Cmd Msg
rollDice =
    Random.int 1 4
        |> Random.andThen
            (\d4 ->
                Random.int 1 6
                    |> Random.andThen
                        (\d6 ->
                            Random.int 1 8
                                |> Random.andThen
                                    (\d8 ->
                                        Random.int 0 9
                                            |> Random.andThen
                                                (\d10 ->
                                                    Random.int 1 12
                                                        |> Random.andThen
                                                            (\d12 ->
                                                                Random.int 1 20
                                                                    |> Random.map
                                                                        (\d20 ->
                                                                            { d4 = d4
                                                                            , d6 = d6
                                                                            , d8 = d8
                                                                            , d10 = d10
                                                                            , d12 = d12
                                                                            , d20 = d20
                                                                            }
                                                                        )
                                                            )
                                                )
                                    )
                        )
            )
        |> Random.generate Rolled


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
    arr


updateSector : (Sector -> Sector) -> Array (Array Sector) -> Coordinates -> Array (Array Sector)
updateSector updateFn arr coords =
    -- TODO: Make the replacement fn WORK
    arrayUpdate
        coords.row
        (\row -> arrayUpdate coords.col updateFn row)
        arr


sectorClicked : Model -> Coordinates -> Model
sectorClicked model coords =
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
                                    case validMapSector t.roll.d10 s l coords of
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
                                    case validResourceScan t.roll.d10 s l coords of
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Message" msg of
        SectorClicked c ->
            ( sectorClicked model c
            , Cmd.none
            )

        RollDice ->
            ( removeHoveredAction model
            , rollDice
            )

        Rolled result ->
            -- TODO: if the d20 is 20 the player is forced into an Anomaly
            ( { model
                | turnState =
                    Just
                        { roll = result
                        , action = NoAction
                        }
              }
            , Cmd.none
            )

        HoveredAction ha ->
            ( { model | hoveredAction = Just ha }
            , Cmd.none
            )

        UnhoveredAction ->
            ( removeHoveredAction model
            , Cmd.none
            )

        SelectedAction sa ->
            ( updateTurnStateAction sa model |> removeHoveredAction
            , Cmd.none
            )
