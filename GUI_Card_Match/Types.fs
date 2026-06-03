module Types

type Card =
    {
        Value : string
        Theme : string
        Revealed : bool
        Matched : bool
    }

type Difficulty =
    | Easy
    | Normal
    | Hard

type GameState =
    {
        Board : Card list
        Size : int
        Attempts : int
        Theme : string
        SeenCards : Map<string, Set<int>>
    }