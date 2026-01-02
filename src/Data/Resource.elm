module Data.Resource exposing (..)

import Types exposing (..)


resourceKindToName : ResourceKind -> String
resourceKindToName rk =
    case rk of
        None ->
            "Nothing"

        Water ->
            "Water"

        RawMetals ->
            "Raw Metals"

        MetalAlloys ->
            "Metal Alloys"

        Silicon ->
            "Silicon"

        DarkMatter ->
            "Dark Matter"

        ExoticMinerals ->
            "Exotic Minerals"


resourceKindToSymbol : ResourceKind -> String
resourceKindToSymbol rk =
    case rk of
        None ->
            "X"

        Water ->
            "W"

        RawMetals ->
            "R"

        MetalAlloys ->
            "M"

        Silicon ->
            "S"

        DarkMatter ->
            "D"

        ExoticMinerals ->
            "E"


intToResourceKind : Int -> ResourceKind
intToResourceKind i =
    case i of
        2 ->
            Water

        3 ->
            RawMetals

        4 ->
            MetalAlloys

        5 ->
            Silicon

        6 ->
            DarkMatter

        7 ->
            ExoticMinerals

        _ ->
            None
