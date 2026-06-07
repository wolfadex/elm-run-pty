module Extra.String exposing (atIndex)


atIndex : Int -> String -> String
atIndex idx str =
    str
        |> String.dropLeft idx
        |> String.left 1
