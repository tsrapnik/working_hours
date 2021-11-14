module Array.Extra2 exposing (updateWithArray)

import Array exposing (Array)


{-| Similar to Array.Extra.update, but in stead of replacing one element (at given index) with another one, it replaces
it with an array of zero or more elements (given by a -> Array a function).
-}
updateWithArray : Int -> (a -> Array a) -> Array a -> Array a
updateWithArray index replacement array =
    let
        left =
            Array.slice 0 index array

        right =
            Array.slice (index + 1) (Array.length array) array

        maybeElement =
            Array.get index array
    in
    case maybeElement of
        Just element ->
            Array.append left (Array.append (replacement element) right)

        Nothing ->
            array
