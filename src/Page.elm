module Page exposing (page)

import Board exposing (board)
import Html exposing (Html)
import Html.Attributes as Attribute exposing (class, title)
import Html.Events as Event
import Types exposing (..)


page : Model -> Html Msg
page model =
    Html.div [ class "font-['Exo_2'] flex justify-center items-center" ]
        [ board model ]
