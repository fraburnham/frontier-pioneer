module Main exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes as Attribute exposing (class)
import Html.Events as Event
import Page exposing (page)
import Types exposing (..)
import Update exposing (update)


main =
    Browser.element
        { init = init
        , update = update
        , view = page
        , subscriptions = subscriptions
        }


initialSectors : Array (Array Sector)
initialSectors =
    Array.repeat maxSectorCol (Array.repeat maxSectorRow Unmapped)


initialUpgrades : UpgradeProgress
initialUpgrades =
    { blinkDrive = 0
    , terraformingTechnology = 0
    , shipImprovements = 0
    , scannerTechnology = 0
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { sectors = initialSectors
      , upgradeProgress = initialUpgrades
      , damage = 0
      , turnState = Nothing
      , location = Nothing
      , hoveredAction = Nothing
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
