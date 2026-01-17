module View.Rules exposing (..)

import Data.Die exposing (dieToString)
import Data.Resource exposing (resourceKindToName)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (class, id, title)
import Types exposing (Die(..), DieUseMeaning, DieUseMeaningValue(..), DieValueMeaning, Model, Msg, ResourceKind(..))


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


upgradeUsableResourceKindsTableRow : Bool -> ResourceKind -> Html Msg
upgradeUsableResourceKindsTableRow bottomBorder rk =
    Html.div
        [ class <|
            if bottomBorder then
                "flex border-r-1 border-black/25"

            else
                "flex"
        ]
        [ Html.div [ class "p-1 px-2" ] [ Html.text <| resourceKindToName rk ] ]


upgradeUsableResourceKindsTableRows : List ResourceKind -> List (Html Msg)
upgradeUsableResourceKindsTableRows valueMeanings =
    case valueMeanings of
        [ rk ] ->
            [ upgradeUsableResourceKindsTableRow False rk ]

        rk :: rest ->
            upgradeUsableResourceKindsTableRow True rk :: upgradeUsableResourceKindsTableRows rest

        [] ->
            []


upgradeUsableResourceKindsTable : List ResourceKind -> Html Msg
upgradeUsableResourceKindsTable valueMeanings =
    Html.div [ class "my-2 flex flex-row w-full" ] <|
        upgradeUsableResourceKindsTableRows valueMeanings


upgradeTableRow : Bool -> String -> String -> List ResourceKind -> Html Msg
upgradeTableRow bottomBorder name benefit usableResourceKinds =
    Html.div
        [ class <|
            if bottomBorder then
                "flex border-b-1 border-black/25"

            else
                "flex"
        ]
        [ Html.div [ class "w-1/4 text-right p-1 pr-2 border-r-1 border-black/25" ] [ Html.text name ]
        , Html.div [ class "w-3/4 p-1 pl-2" ]
            [ Html.text benefit
            , Html.div [ class "flex flex-row justify-around" ]
                [ upgradeUsableResourceKindsTable usableResourceKinds ]
            ]
        ]


upgradeTableRows : List ( String, String, List ResourceKind ) -> List (Html Msg)
upgradeTableRows upgradeDetails =
    case upgradeDetails of
        [ ( name, benefit, usableResourceKinds ) ] ->
            [ upgradeTableRow False name benefit usableResourceKinds ]

        ( name, benefit, usableResourceKinds ) :: rest ->
            upgradeTableRow True name benefit usableResourceKinds :: upgradeTableRows rest

        [] ->
            []


upgradeTable : List ( String, String, List ResourceKind ) -> Html Msg
upgradeTable upgradeDetails =
    Html.div [ class "my-2 px-1" ] <|
        upgradeTableRows upgradeDetails


movement : Model -> Html Msg
movement model =
    rulesSection "Movement"
        [ Html.div [ class "flex flex-row" ]
            [ Html.text "When you pass through a sector you can collect the resources in it. Entering a sector"
            , Html.div [ class "italic mx-[0.20rem]" ] [ Html.text "does not" ]
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
             Once a sector has been mapped it can be scanned for resources which can be collected by moving to that sector. You can scan the sector you're occupying.
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


upgrades : Model -> Html Msg
upgrades model =
    rulesSection "Upgrades"
        [ Html.text "Upgrades require 20 resources each to enable."
        , upgradeTable
            [ ( "Blink Drive", "Double movement points.", [ DarkMatter, ExoticMinerals, Water ] )
            , ( "Terraforming Technology", "Use larger of d12 or d20 for quantity when resource scanning.", [ RawMetals, MetalAlloys, Water ] )
            , ( "Ship Repairs", "Increase resources discovered by 2 and decrease damage received by 1.", [ Silicon, MetalAlloys, Water ] )
            , ( "Scanner Technology", "Automatically map and scan for resources when entering a sector.", [ Silicon, ExoticMinerals, Water ] )
            ]
        ]


anomaly : Model -> Html Msg
anomaly model =
    rulesSection "Anomalies"
        [ Html.text "When a 20 is rolled all players are forced to take the Anomaly action."
        , dieUseTable
            [ ( D4, "Anomaly Range", Simple "Determines the area of effect of the anomaly." )
            , ( D8
              , "Anomaly Type"
              , Formatted
                    [ Html.text "Determines the area of effect of the anomaly."
                    , dieValueTable
                        [ ( 1, "Space Rift (forces movement of d6 spaces)" )
                        , ( 2, "Energy Surge (reduces mapping and scanning range by 2)" )
                        , ( 3, "Asteroid Shower (causes d6 ship damage)" )
                        , ( 4, "Gravitational Distortion (destroys resources within half of d4 range)" )
                        , ( 5, "Temporal Distortion (doubles the cost of movement next turn)" )
                        , ( 6, "Alien Signal (causes mapping and scanning to fail next turn)" )
                        , ( 7, "Alien Encounter (causes d6 ship damage)" )
                        , ( 8, "Space Pirates (takes resources from d6 different sectors within d4 range)" )
                        ]
                    ]
              )
            ]
        ]


overview : Html Msg
overview =
    rulesSection "Overview"
        [ Html.text
            """
            Frontier Pioneer is a simultaneous, roll-and-write game with a fixed number of rounds where players explore procedurally generated space
sectors, scan for resources, and upgrade their ships. Players do not take turns; instead, they independently choose actions based on the
results of a shared dice pool. There are 30 rolls total for approximately 15min of play time.
            """
        ]


rules : Model -> Html Msg
rules model =
    Html.div
        [ id "rules"
        , class "flex justify-start mb-8 w-full p-2 overflow-y-auto"
        ]
        [ Html.div []
            [ overview
            , movement model
            , mapping model
            , resourceDiscovery model
            , anomaly model
            , upgrades model

            -- scoring
            ]
        ]



-- FIX: die numbers (d4/etc) aren't aligning in the table
