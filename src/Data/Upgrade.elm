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


upgradeToEffect : Upgrade -> Effect
upgradeToEffect upgrade =
    case upgrade of
        BlinkDrive ->
            MovementImproved

        TerraformingTech ->
            ResourceDiscoveryImproved

        ShipRepairs ->
            ShipImproved

        ScannerTech ->
            ScanningImproved


upgradeProgress : Upgrade -> Model -> Int
upgradeProgress upgrade model =
    case upgrade of
        BlinkDrive ->
            model.upgradeProgress.blinkDrive

        TerraformingTech ->
            model.upgradeProgress.terraformingTech

        ShipRepairs ->
            model.upgradeProgress.shipRepairs

        ScannerTech ->
            model.upgradeProgress.scannerTech
