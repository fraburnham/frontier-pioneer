module View.Map exposing (map)

import Array exposing (Array)
import Data.Effect exposing (..)
import Data.Resource exposing (..)
import Data.Sector exposing (..)
import Html exposing (Html)
import Html.Attributes as Attribute exposing (class, id, title)
import Html.Events exposing (onClick)
import Rules exposing (..)
import Types exposing (..)


sectorStyleMapped : Model -> Sector -> String
sectorStyleMapped model s =
    case s of
        Unmapped ->
            "divide-gray-300"

        Mapped _ ->
            "divide-gray-500 hover:font-medium"


sectorStyleCurrentLocation : Model -> Int -> Int -> String
sectorStyleCurrentLocation model col row =
    case model.location of
        Nothing ->
            ""

        Just l ->
            case l.col == col && l.row == row of
                True ->
                    "bg-sky-100"

                False ->
                    ""


sectorStyleValidForAction : Model -> Int -> Int -> Sector -> String
sectorStyleValidForAction model col row s =
    let
        validStyle =
            "!bg-gray-200"

        invalidStyle =
            ""

        coords =
            { row = row, col = col }

        effects =
            activeEffects model
    in
    case model.location of
        Nothing ->
            validStyle

        Just l ->
            case model.turnState of
                Nothing ->
                    invalidStyle

                Just t ->
                    case model.hoveredAction of
                        -- TODO: handle deciding which validator to use differently. A lookup maybe? That way I can dispatch differently for hover and selected
                        --       yeah. Then these calls can be valid Action model turnState location coords (or whatever)
                        Just ha ->
                            case ha of
                                Move ml ->
                                    case validMoveHover ml l coords of
                                        True ->
                                            validStyle

                                        False ->
                                            invalidStyle

                                MapSector ->
                                    case validMapSector effects t.roll.d10 s l coords of
                                        True ->
                                            validStyle

                                        False ->
                                            invalidStyle

                                ResourceScan ->
                                    case validResourceScan effects t.roll.d10 s l coords of
                                        True ->
                                            validStyle

                                        False ->
                                            invalidStyle

                                _ ->
                                    invalidStyle

                        Nothing ->
                            case t.action of
                                NoAction ->
                                    invalidStyle

                                a ->
                                    case a of
                                        Move ml ->
                                            case validMove ml l coords of
                                                True ->
                                                    validStyle

                                                False ->
                                                    invalidStyle

                                        MapSector ->
                                            case validMapSector effects t.roll.d10 s l coords of
                                                True ->
                                                    validStyle

                                                False ->
                                                    invalidStyle

                                        ResourceScan ->
                                            case validResourceScan effects t.roll.d10 s l coords of
                                                True ->
                                                    validStyle

                                                False ->
                                                    invalidStyle

                                        _ ->
                                            invalidStyle


sectorStyle : Model -> Int -> Int -> Sector -> String
sectorStyle model col row s =
    String.join " "
        [ "flex flex-col size-fit divide-y-1"
        , sectorStyleMapped model s
        , sectorStyleCurrentLocation model col row
        , sectorStyleValidForAction model col row s
        ]


sectorRowStyle : Sector -> String
sectorRowStyle s =
    case s of
        Unmapped ->
            "flex flex-row divide-x-1 divide-gray-300"

        Mapped _ ->
            "flex flex-row divide-x-1 divide-gray-500"


sectorBoxStyle : String
sectorBoxStyle =
    "h-[1.5rem] w-[1.5rem] flex items-center justify-center"


sector : Model -> Int -> Int -> Sector -> Html Msg
sector model row col data =
    let
        sectorRowStyleResolved =
            sectorRowStyle data

        sectorAttributes =
            [ class (sectorStyle model col row data)
            , onClick
                (SectorClicked
                    { row = row
                    , col = col
                    }
                )
            ]
    in
    case data of
        Unmapped ->
            Html.div sectorAttributes
                [ Html.div [ class sectorRowStyleResolved ]
                    [ Html.div [ class sectorBoxStyle ]
                        []
                    , Html.div [ class sectorBoxStyle ]
                        []
                    ]
                , Html.div [ class sectorRowStyleResolved ]
                    [ Html.div [ class sectorBoxStyle ]
                        []
                    , Html.div [ class sectorBoxStyle ]
                        []
                    ]
                ]

        Mapped d ->
            Html.div sectorAttributes
                [ Html.div [ class sectorRowStyleResolved ]
                    [ Html.div [ class sectorBoxStyle ]
                        []
                    , Html.div
                        [ class sectorBoxStyle
                        , title (sectorKindToName d.kind)
                        ]
                        [ Html.text (sectorKindToSymbol d.kind) ]
                    ]
                , case d.resource of
                    Undiscovered ->
                        Html.div [ class sectorRowStyleResolved ]
                            [ Html.div [ class sectorBoxStyle ]
                                []
                            , Html.div [ class sectorBoxStyle ]
                                []
                            ]

                    Discovered rd ->
                        Html.div [ class sectorRowStyleResolved ]
                            [ Html.div [ class sectorBoxStyle ]
                                [ Html.text (String.fromInt rd.count) ]
                            , Html.div
                                [ class sectorBoxStyle
                                , title (resourceKindToName rd.kind)
                                ]
                                [ Html.text (resourceKindToSymbol rd.kind) ]
                            ]
                ]


mapRow : Model -> Int -> Array Sector -> Html Msg
mapRow model row sectors =
    Html.div [ class "flex flex-row divide-x-1 size-fit" ]
        (Array.indexedMap (sector model row) sectors |> Array.toList)


map : Model -> Html Msg
map model =
    Html.div
        [ id "map"
        , class "flex flex-col border-1 divide-y-1 size-fit m-2 bg-sky-50"
        ]
        (Array.indexedMap (mapRow model) model.sectors |> Array.toList)
