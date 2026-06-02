namespace GUI_Card_Match.Models

module Logic

let revealCard board index =

    board
    |> List.mapi (fun i card ->

        if i = index then

            {
                card with
                    IsRevealed = true
            }

        else

            card)

let hideCards board i1 i2 =

    board
    |> List.mapi (fun i card ->

        if i = i1 || i = i2 then

            {
                card with
                    IsRevealed = false
            }

        else

            card)

let markMatched board i1 i2 =

    board
    |> List.mapi (fun i card ->

        if i = i1 || i = i2 then

            {
                card with
                    IsMatched = true
            }

        else

            card)

let isFinished board =

    board
    |> List.forall (fun c ->
        c.IsMatched)