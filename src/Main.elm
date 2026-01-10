module Main exposing (main)

import Array exposing (Array)
import Browser
import Data.Sector exposing (..)
import Html exposing (Html)
import Html.Attributes as Attribute exposing (class)
import Html.Events as Event
import Types exposing (..)
import Update exposing (update)
import View.Board exposing (board)


main =
    Browser.element
        { init = init
        , update = update
        , view = board
        , subscriptions = subscriptions
        }


initialSectors : Array (Array Sector)
initialSectors =
    Array.repeat maxSectorCol (Array.repeat maxSectorRow Unmapped)


initialUpgrades : UpgradeProgress
initialUpgrades =
    { blinkDrive = 0
    , terraformingTech = 0
    , shipRepairs = 0
    , scannerTech = 0
    }


init : {} -> ( Model, Cmd Msg )
init _ =
    ( { sectors = initialSectors
      , upgradeProgress = initialUpgrades
      , damage = 0
      , turnState = Nothing
      , location = Nothing
      , hoveredAction = Nothing
      , effects = []
      , temporaryEffect = Nothing
      , turnNumber = 0
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
