module Data.Upgrade exposing (..)

import Types exposing (..)


upgradeToName : Upgrade -> String
upgradeToName upgrade =
    case upgrade of
        BlinkDrive ->
            "Blink Drive"

        TerraformingTech ->
            "Terraforming Tech"

        ShipRepairs ->
            "Ship Repairs"

        ScannerTech ->
            "Scanner Tech"
