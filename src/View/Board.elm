module View.Board exposing (board)

import Html exposing (Html)
import Html.Attributes as Attribute exposing (class, id, title)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Rules exposing (..)
import Types exposing (..)
import View.Map exposing (map)


header : Html Msg
header =
    Html.div [ class "font-[Russo_One] text-4xl text-center pt-4 text-shadow-xs" ]
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
    Html.div [ class "flex h-[5rem] items-center text-center justify-around" ] <|
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


actionButtons : Model -> Html Msg
actionButtons model =
    let
        buttonStyle =
            class "flex items-center justify-center border-1 rounded size-fit p-2 w-1/4 h-full hover:font-medium hover:border-2"

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

                            -- TODO: a fn that gets the right number to move (from rules) then I think I can drop the difference between the hover and click style
                            --       by putting the right number in place at the beginning (though having the difference shows how _far_ you can move and is interesting)
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


actionHint : String -> Html Msg
actionHint text =
    Html.div [ class "flex italic font-medium text-center items-center justify-center h-[3rem] w-full wrap" ]
        [ Html.text text ]


anomalyMessage : TurnState -> String
anomalyMessage t =
    case t.roll.d8 of
        1 ->
            "You have encountered a space rift. You must move " ++ String.fromInt t.roll.d6 ++ " spaces."

        2 ->
            "You have encountered an energy surge. Your scanning and mapping ranges will be reduced next turn."

        3 ->
            "You have encountered an asteroid shower. Your ship suffered " ++ String.fromInt t.roll.d6 ++ " damage."

        4 ->
            "You have encountered a gravitational distortion. Resources within " ++ String.fromInt (max 1 (t.roll.d4 // 2)) ++ " spaces are destroyed."

        5 ->
            "You have encountered a temporal distortion. The cost of movement will be doubled next turn."

        6 ->
            "You have encountered an alien signal. It disrupted your scanners. You will not be able to scan for resources or map sectors next turn."

        7 ->
            "You have encountered a alien vessel. They attack your ship. Your ship suffers " ++ String.fromInt t.roll.d6 ++ " damage."

        8 ->
            "You have encountered pirates. They have pillaged several nearby sectors of resources."

        _ ->
            "You have encountered an anomaly."


actionArea : Model -> Html Msg
actionArea model =
    Html.div [ class "flex flex-col mb-4 h-[8rem]" ]
        (case model.location of
            Nothing ->
                [ Html.div [ class "flex italic font-medium text-center items-center justify-center h-full w-full" ]
                    [ Html.text "Select a starting sector to begin" ]
                ]

            Just _ ->
                case model.turnState of
                    Just t ->
                        case t.action of
                            NoAction ->
                                [ dice model
                                , actionButtons model
                                ]

                            Move spacesLeft ->
                                [ dice model
                                , actionHint ("Select your next sector. You cannot move diagonally. You have " ++ String.fromInt spacesLeft ++ " moves left.")
                                ]

                            MapSector ->
                                [ dice model
                                , actionHint ("Select a sector to map within " ++ String.fromInt t.roll.d10 ++ " spaces.")
                                ]

                            ResourceScan ->
                                [ dice model
                                , actionHint ("Select a sector to scan for resources within " ++ String.fromInt t.roll.d10 ++ " spaces.")
                                ]

                            Anomaly ->
                                [ Html.div [ class "flex items-center h-[5rem]" ]
                                    [ actionHint <| anomalyMessage t ]

                                -- TODO: make this show all the details of the anomaly
                                , actionButtons model
                                ]

                    Nothing ->
                        [ dice model
                        , actionButtons model
                        ]
        )


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
                    [ Html.div [] [ Html.text "Upgrades" ]
                    , Html.div [] [ Html.text "Damage" ]
                    , Html.div [] [ Html.text "Score" ]
                    , Html.div [] [ Html.text "Roll history" ]
                    , Html.div [] [ Html.text "Rules" ]
                    ]
                ]
            ]
        ]



-- NEXT: handle collecting resources
-- NEXT: handle upgrades
-- NEXT: handle special movement costs (nebula, enemy space)
-- NEXT: handle resource limitations (no resources in enemy space; X for count when kind is None)
-- NEXT: handle endgame (turn counting)/scoring!
-- NEXT: handle showing active effects
-- NEXT: ability to abort action somehow... (maybe trap esc and have the help hint show "Press ESC to abort")
-- NEXT: handle roll history
-- NEXT: handle rules display
-- NEXT: don't allow selecting an option if there will be _zero_ valid moves for it
-- NEXT: put the "would be state" in the sector but blurred when there is an action selected or hovered
-- Tally marks!
-- 𝍩
-- 𝍪
-- 𝍫
-- 𝍬
-- 𝍸
