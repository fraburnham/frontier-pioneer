module View.Rules exposing (..)

import Data.Die exposing (dieToString)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (class, id, title)
import Types exposing (Die(..), DieUseMeaning, DieUseMeaningValue(..), DieValueMeaning, Model, Msg)


details : List (Attribute msg) -> List (Html msg) -> Html msg
details attributes children =
    Html.node "details" attributes children


summary : List (Attribute msg) -> List (Html msg) -> Html msg
summary attributes children =
    Html.node "summary" attributes children


rulesSection : String -> List (Html msg) -> Html msg
rulesSection title children =
    Html.div
        [ String.toLower title |> String.replace " " "-" |> id
        , class "flex flex-col items-center my-2"
        ]
        [ Html.div [ class "font-medium flex items-start w-full border-b-1 border-black/50" ] [ Html.text title ]
        , Html.div [ class "mt-1 px-[1rem] w-full font-light" ] children
        ]


dieValueTableRow : Bool -> Int -> String -> Html Msg
dieValueTableRow bottomBorder number meaning =
    Html.div
        [ class <|
            if bottomBorder then
                "flex border-b-1 border-black/25"

            else
                "flex"
        ]
        [ Html.div [ class "w-1/4 text-right p-1 pr-2 border-r-1 border-black/25" ] [ Html.text <| String.fromInt number ]
        , Html.div [ class "w-3/4 p-1 pl-2" ] [ Html.text meaning ]
        ]


dieValueTableRows : DieValueMeaning -> List (Html Msg)
dieValueTableRows valueMeanings =
    case valueMeanings of
        [ ( num, str ) ] ->
            [ dieValueTableRow False num str ]

        ( num, str ) :: rest ->
            dieValueTableRow True num str :: dieValueTableRows rest

        [] ->
            []


dieValueTable : DieValueMeaning -> Html Msg
dieValueTable valueMeanings =
    Html.div [ class "my-2 px-1" ] <|
        dieValueTableRows valueMeanings


dieUseTableRow : Bool -> Die -> String -> DieUseMeaningValue -> Html Msg
dieUseTableRow bottomBorder die name meaning =
    Html.div
        [ class <|
            if bottomBorder then
                "flex-row flex border-b-1 border-black/35 w-full"

            else
                "flex-row flex w-full"
        ]
        [ Html.div [ class "p-1 pr-2 border-r-1 border-black/35 w-[2.5rem] text-right" ] [ Html.text <| dieToString die ]
        , Html.div [ class "p-1 text-center w-[8rem]" ] [ Html.text name ]
        , Html.div [ class "p-1 border-l-1 border-black/35 pl-2 w-[40rem]" ] <|
            case meaning of
                Simple s ->
                    [ Html.text s ]

                Formatted h ->
                    h
        ]


dieUseTableRows : DieUseMeaning -> List (Html Msg)
dieUseTableRows useMeanings =
    case useMeanings of
        [ ( die, name, meaning ) ] ->
            [ dieUseTableRow False die name meaning ]

        ( die, name, meaning ) :: rest ->
            dieUseTableRow True die name meaning :: dieUseTableRows rest

        [] ->
            []


dieUseTable : DieUseMeaning -> Html Msg
dieUseTable useMeanings =
    Html.div [ class "flex flex-col mt-2" ] <|
        dieUseTableRows useMeanings


movement : Model -> Html Msg
movement model =
    rulesSection "Movement"
        [ Html.div [ class "flex flex-row" ]
            [ Html.text "When you pass through a sector you can collect the resources in it. Entering a sector"
            , Html.div [ class "font-medium mx-[0.20rem]" ] [ Html.text "does not" ]
            , Html.text "map it."
            ]
        , dieUseTable [ ( D4, "Warp Drive", Simple "Determines the maximum number of sectors a player can move." ) ]
        ]


mapping : Model -> Html Msg
mapping model =
    let
        ruleText =
            """
            You must map a sector before you're able to scan it for resources. Choose one sector within the range determined by the scan range and mark it as having the sector type.
            """
    in
    rulesSection "Mapping"
        [ Html.text ruleText
        , dieUseTable
            [ ( D10, "Scan Range", Simple "Determines how far the mapping scan reaches." )
            , ( D6
              , "Sector Type"
              , Formatted <|
                    [ Html.text "Determines the type of sector that is mapped."
                    , dieValueTable
                        [ ( 1, "Deep Space" )
                        , ( 2, "Colonized Star System" )
                        , ( 3, "Uncolonized Star System" )
                        , ( 4, "Nebula (Requires 2 movement points to exit)" )
                        , ( 5, "Enemy Space (Suffer 2 damage to enter)" )
                        , ( 6, "Deep Space" )
                        ]
                    ]
              )
            ]
        ]


resourceDiscovery : Model -> Html Msg
resourceDiscovery model =
    let
        ruleText =
            """
             Once a sector has been discovered it can be scanned for resources which can be collected by moving to that sector. You can scan the sector you're occupying.
            """
    in
    rulesSection "Resource Discovery"
        [ Html.text ruleText
        , dieUseTable
            [ ( D10, "Scan Range", Simple "Determines how far the resource scan reaches." )
            , ( D12, "Resource Quantity", Simple "Determines the amount of the resource discovered." )
            , ( D8
              , "Resource Type"
              , Formatted <|
                    [ Html.text "Determines the type of resource discovered."
                    , dieValueTable
                        [ ( 1, "Nothing" )
                        , ( 2, "Water (never found in Deep Space)" )
                        , ( 3, "Raw Metals" )
                        , ( 4, "Metal Alloys" )
                        , ( 5, "Silicon" )
                        , ( 6, "Dark Matter (never found in a Colonized System)" )
                        , ( 7, "Exotic Minerals (never found in a Colonized System)" )
                        , ( 8, "Nothing" )
                        ]
                    ]
              )
            ]
        ]


actions : Model -> Html Msg
actions model =
    Html.div [ id "action-rules" ]
        [ movement model
        , mapping model
        , resourceDiscovery model
        ]


rules : Model -> Html Msg
rules model =
    Html.div
        -- TODO: make this container have scrollbar(s?) mmm but only in 2xl
        [ id "rules"
        , class "flex flex-row justify-start mb-8 w-full p-2 overflow-y-auto"
        ]
        [ actions model
        , Html.div [ id "upgrades-rules" ] []
        , Html.div [ id "anomaly-rules" ] []
        ]



-- FIX: die numbers (d4/etc) aren't aligning in the table
