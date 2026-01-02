module View.Board exposing (board)

import Data.Damage exposing (..)
import Data.Effect exposing (..)
import Data.Sector exposing (..)
import Data.Upgrade exposing (..)
import Html exposing (Html)
import Html.Attributes as Attribute exposing (class, id, title)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Rules exposing (..)
import Types exposing (..)
import View.Map exposing (map)


header : Html Msg
header =
    Html.div [ class "font-[Russo_One] text-4xl text-center pt-2 text-shadow-xs" ]
        [ Html.text "Frontier Pioneer" ]


dieStyle : Model -> Die -> String
dieStyle model d =
    let
        activeStyle =
            ""

        inactiveStyle =
            "blur-[1px] opacity-50"
    in
    case model.turnState of
        Nothing ->
            inactiveStyle

        Just t ->
            case model.hoveredAction of
                Nothing ->
                    activeStyle

                Just ha ->
                    case ha of
                        Move _ ->
                            case d of
                                D4 ->
                                    activeStyle

                                _ ->
                                    inactiveStyle

                        MapSector ->
                            case d of
                                D6 ->
                                    activeStyle

                                D10 ->
                                    activeStyle

                                _ ->
                                    inactiveStyle

                        ResourceScan ->
                            case d of
                                D8 ->
                                    activeStyle

                                D10 ->
                                    activeStyle

                                D12 ->
                                    activeStyle

                                _ ->
                                    inactiveStyle

                        _ ->
                            activeStyle


die : String -> String -> Maybe Int -> Html Msg
die style kind val =
    Html.div [ class style ]
        [ Html.div [ class "border-b-1 border-black/50 font-light" ] [ Html.text kind ]
        , Html.div []
            [ Html.text
                (case val of
                    Nothing ->
                        "?"

                    Just v ->
                        String.fromInt v
                )
            ]
        ]


dice : Model -> Html Msg
dice model =
    let
        style =
            dieStyle model
    in
    Html.div [ class "flex h-[4rem] items-center text-center justify-around" ] <|
        case model.turnState of
            Nothing ->
                [ die (style D4) "d4" Nothing
                , die (style D6) "d6" Nothing
                , die (style D8) "d8" Nothing
                , die (style D10) "d10" Nothing
                , die (style D12) "d12" Nothing
                , die (style D20) "d20" Nothing
                ]

            Just t ->
                [ die (style D4) "d4" (Just t.roll.d4)
                , die (style D6) "d6" (Just t.roll.d6)
                , die (style D8) "d8" (Just t.roll.d8)
                , die (style D10) "d10" (Just t.roll.d10)
                , die (style D12) "d12" (Just t.roll.d12)
                , die (style D20) "d20" (Just t.roll.d20)
                ]


buttonStyle : Html.Attribute Msg
buttonStyle =
    class "flex items-center justify-center border-1 rounded size-fit py-2 w-1/4 h-full hover:font-medium hover:border-2 mx-1"


actionButtons : Model -> Html Msg
actionButtons model =
    let
        rollButton =
            [ Html.div
                [ buttonStyle
                , onClick RollDice
                ]
                [ Html.text "Roll" ]
            ]
    in
    Html.div [ class "flex justify-around items-center text-center w-full h-[3rem]" ] <|
        case model.turnState of
            Nothing ->
                rollButton

            Just t ->
                case t.action of
                    Anomaly ->
                        rollButton

                    _ ->
                        let
                            moveDistance =
                                movementDistanceModifier (activeEffects model) t.roll.d4
                        in
                        [ Html.div
                            [ buttonStyle
                            , onClick (SelectedAction (Move moveDistance))
                            , onMouseEnter (HoveredAction (Move moveDistance))
                            , onMouseLeave UnhoveredAction
                            ]
                            [ Html.text "Move" ]
                        , Html.div
                            [ buttonStyle
                            , onClick (SelectedAction MapSector)
                            , onMouseEnter (HoveredAction MapSector)
                            , onMouseLeave UnhoveredAction
                            ]
                            [ Html.text "Map Sector" ]
                        , Html.div
                            [ buttonStyle
                            , onClick (SelectedAction ResourceScan)
                            , onMouseEnter (HoveredAction ResourceScan)
                            , onMouseLeave UnhoveredAction
                            ]
                            [ Html.text "Resource Scan" ]
                        ]


collectResourceButton : Coordinates -> ResourceData -> Upgrade -> Html Msg
collectResourceButton l r applyTo =
    Html.div
        [ buttonStyle
        , onClick
            (ResourceCollected
                { data = r
                , location = l
                , applyTo = applyTo
                }
            )
        ]
        [ Html.text (upgradeToName applyTo) ]


collectResourceButtons : Coordinates -> ResourceData -> Html Msg
collectResourceButtons l r =
    let
        blinkDrive =
            collectResourceButton l r BlinkDrive

        terraformingTech =
            collectResourceButton l r TerraformingTech

        shipRepairs =
            collectResourceButton l r ShipRepairs

        scannerTech =
            collectResourceButton l r ScannerTech
    in
    Html.div [ class "flex justify-around items-center text-center w-full h-[3rem]" ] <|
        case r.kind of
            None ->
                []

            Water ->
                [ blinkDrive
                , terraformingTech
                , shipRepairs
                , scannerTech
                ]

            RawMetals ->
                [ terraformingTech ]

            MetalAlloys ->
                [ terraformingTech
                , shipRepairs
                ]

            Silicon ->
                [ shipRepairs
                , scannerTech
                ]

            DarkMatter ->
                [ blinkDrive ]

            ExoticMinerals ->
                [ blinkDrive
                , scannerTech
                ]


actionHint : String -> Html Msg
actionHint text =
    Html.div [ class "flex italic font-medium text-center items-center justify-center h-[5rem] w-full wrap" ]
        [ Html.text text ]


anomalyMessage : Model -> TurnState -> String
anomalyMessage model t =
    case t.roll.d8 of
        1 ->
            "You have encountered a space rift. You must move " ++ String.fromInt t.roll.d6 ++ " spaces."

        2 ->
            "You have encountered an energy surge. Your scanning and mapping ranges will be reduced next turn."

        3 ->
            "You have encountered an asteroid shower. Your ship suffered " ++ String.fromInt (damageAmount model t) ++ " damage."

        4 ->
            "You have encountered a gravitational distortion. Resources within " ++ String.fromInt (max 1 (t.roll.d4 // 2)) ++ " spaces are destroyed."

        5 ->
            "You have encountered a temporal distortion. The cost of movement will be doubled next turn."

        6 ->
            "You have encountered an alien signal. It disrupted your scanners. You will not be able to scan for resources or map sectors next turn."

        7 ->
            "You have encountered a alien vessel. They attack your ship. Your ship suffers " ++ String.fromInt (damageAmount model t) ++ " damage."

        8 ->
            "You have encountered pirates. They have pillaged several nearby sectors of resources."

        _ ->
            "You have encountered an anomaly."


activeAction : Model -> TurnState -> List (Html Msg)
activeAction model t =
    case t.action of
        NoAction ->
            [ dice model
            , actionButtons model
            ]

        Move spacesLeft ->
            [ actionHint ("Select your next sector. You cannot move diagonally. You have " ++ String.fromInt spacesLeft ++ " moves left.") ]

        MapSector ->
            [ actionHint ("Select a sector to map within " ++ String.fromInt t.roll.d10 ++ " spaces.")
            , Html.div [] []
            ]

        ResourceScan ->
            [ actionHint ("Select a sector to scan for resources within " ++ String.fromInt t.roll.d10 ++ " spaces.")
            , Html.div [] []
            ]

        Anomaly ->
            [ Html.div [ class "flex items-center h-[5rem]" ]
                [ actionHint <| anomalyMessage model t ]
            , actionButtons model
            ]


actionArea : Model -> Html Msg
actionArea model =
    let
        handleTurnState =
            case model.turnState of
                Just t ->
                    activeAction model t

                Nothing ->
                    [ dice model
                    , actionButtons model
                    ]
    in
    Html.div [ class "flex flex-col h-[9rem] justify-center" ] <|
        case model.location of
            Nothing ->
                [ Html.div [ class "flex italic font-medium text-center items-center justify-center h-full w-full" ]
                    [ Html.text "Select a starting sector to begin." ]
                ]

            Just l ->
                case getCurrentSector model of
                    Nothing ->
                        handleTurnState

                    Just s ->
                        case s.resource of
                            Undiscovered ->
                                handleTurnState

                            Discovered r ->
                                case r.count of
                                    0 ->
                                        handleTurnState

                                    _ ->
                                        [ actionHint "Select an upgrade to spend these resources on."
                                        , collectResourceButtons l r
                                        ]


intToTallyMarks : Int -> String
intToTallyMarks i =
    case i of
        0 ->
            ""

        1 ->
            "𝍩"

        2 ->
            "𝍪"

        3 ->
            "𝍫"

        4 ->
            "𝍬"

        5 ->
            "𝍸"

        _ ->
            "𝍸 " ++ intToTallyMarks (i - 5)


upgradeTrackingArea : Model -> Html Msg
upgradeTrackingArea model =
    let
        rowStyle =
            "flex flex-row justify-around h-[4rem]"

        cellStyle =
            "flex flex-col w-1/3 items-center"

        headerStyle =
            "flex size-fit justify-center border-b-1 border-black/50"

        tallyStyle =
            "text-2xl"
    in
    Html.div [ class "flex flex-col my-2" ]
        [ Html.div [ class rowStyle ]
            [ Html.div [ class cellStyle ]
                [ Html.div [ class headerStyle ] [ Html.text "Blink Drive" ]
                , Html.div [ class tallyStyle ] [ Html.text <| intToTallyMarks model.upgradeProgress.blinkDrive ]
                ]
            , Html.div [ class cellStyle ]
                [ Html.div [ class headerStyle ] [ Html.text "Terraforming Tech" ]
                , Html.div [ class tallyStyle ] [ Html.text <| intToTallyMarks model.upgradeProgress.terraformingTech ]
                ]
            ]
        , Html.div [ class rowStyle ]
            [ Html.div [ class cellStyle ]
                [ Html.div [ class headerStyle ] [ Html.text "Ship Repairs" ]
                , Html.div [ class tallyStyle ] [ Html.text <| intToTallyMarks model.upgradeProgress.shipRepairs ]
                ]
            , Html.div [ class cellStyle ]
                [ Html.div [ class headerStyle ] [ Html.text "Scanner Tech" ]
                , Html.div [ class tallyStyle ] [ Html.text <| intToTallyMarks model.upgradeProgress.scannerTech ]
                ]
            ]
        ]


board : Model -> Html Msg
board model =
    Html.div [ class "font-['Exo_2'] flex justify-center items-center" ]
        [ Html.div
            [ id "board"
            , class "flex flex-col size-fit p-4"
            ]
            [ header
            , actionArea model
            , Html.div [ class "flex flex-col w-full items-center" ]
                [ map model

                -- TODO: this div isn't quite right. I don't want this stuff to move when the actionHint is too long
                , Html.div [ class "flex flex-col w-full" ]
                    [ upgradeTrackingArea model

                    -- , Html.div [] [ Html.text "Damage" ]
                    -- , Html.div [] [ Html.text "Score" ]
                    -- , Html.div [] [ Html.text "Roll history" ]
                    -- , Html.div [] [ Html.text "Rules" ]
                    ]
                ]
            ]
        ]



-- NEXT: handle special movement costs (nebula, enemy space)
-- NEXT: handle resource limitations (no resources in enemy space; X for count when kind is None)
-- NEXT: handle endgame (turn counting)/scoring!
-- PLAYABLE!
-- NEXT: don't allow sector clicking to advance the move if there are resources to collect
-- NEXT: action buttons are only clickable if there is at least one valid sector
-- NEXT: some reactive stuff like showing the board and info sections side by side for wide enough viewports
-- NEXT: tests (for the update logic at least, and ideally for the data handling stuff, board is the only skippable part and only if it is _very_ complex)
-- NEXT: refactor. There is lots of sprawl. Can any of it be reduced?
-- NEXT: handle showing active effects
-- NEXT: ability to abort action somehow... (maybe trap esc and have the help hint show "Press ESC to abort")
-- NEXT: handle roll history
-- NEXT: handle rules display
-- NEXT: don't allow selecting an option if there will be _zero_ valid moves for it
-- NEXT: put the "would be state" in the sector but blurred when there is an action selected or hovered
-- NEXT: when hovering over resource buttons change the action hint to be the benefit of each upgrade
-- NEXT: handle multiplayer by pre-generating rolls and allowing them to be exported to a file
