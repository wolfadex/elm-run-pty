module Extra.List exposing
    ( ContinueOrBreak(..)
    , foldlOrBreak
    )


type ContinueOrBreak a
    = Continue a
    | Break a


foldlOrBreak : (a -> b -> ContinueOrBreak b) -> b -> List a -> b
foldlOrBreak fn initial list =
    case list of
        [] ->
            initial

        next :: rest ->
            case fn next initial of
                Break b ->
                    b

                Continue b ->
                    foldlOrBreak fn b rest
