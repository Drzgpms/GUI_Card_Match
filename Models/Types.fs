namespace GUI_Card_Match.Models

type Difficulty =
    | Easy
    | Normal
    | Hard

type Card =
    {
        Value : string
        ImagePath : string

        IsRevealed : bool
        IsMatched : bool
    }

type GameState =
    {
        Board : Card list

        Size : int

        Attempts : int

        Theme : string

        SeenCards : Map<string, Set<int>>
    }