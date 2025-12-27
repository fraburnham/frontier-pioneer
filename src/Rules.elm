module Rules exposing (..)

import Types exposing (..)


gameDistance : Coordinates -> Coordinates -> Int
gameDistance a b =
    abs (a.row - b.row) + abs (a.col - b.col)


validMove : Int -> Coordinates -> Coordinates -> Bool
validMove movesLeft curLocation newLocation =
    (movesLeft > 0) && (gameDistance curLocation newLocation == 1)


validMapSector : Int -> Sector -> Coordinates -> Coordinates -> Bool
validMapSector range sector curLocation sectorLocation =
    (gameDistance curLocation sectorLocation <= range)
        && (case sector of
                Mapped _ ->
                    False

                Unmapped ->
                    True
           )


validResourceScan : Int -> Sector -> Coordinates -> Coordinates -> Bool
validResourceScan range sector curLocation sectorLocation =
    (gameDistance curLocation sectorLocation <= range)
        && (case sector of
                Unmapped ->
                    False

                Mapped s ->
                    True
                        && (case s.resource of
                                Discovered _ ->
                                    False

                                Undiscovered ->
                                    True
                           )
           )
