module AI

open Types

let findKnownPair (state : GameState) =

    state.SeenCards
    |> Map.tryPick (fun _ indices ->

        let validIndices =
            indices
            |> Set.toList
            |> List.filter (fun i ->
                not state.Board[i].Matched)

        match validIndices with
        | a :: b :: _ ->
            Some(a, b)

        | _ ->
            None
    )

let findUnknownCard (state : GameState) =

    state.Board
    |> List.indexed
    |> List.tryFind (fun (i, card) ->

        not card.Matched
        &&
        not (
            state.SeenCards
            |> Map.exists (fun _ indices ->
                Set.contains i indices
            )
        )
    )