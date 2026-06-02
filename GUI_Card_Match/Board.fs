module Board

open Types
open Utils
open Themes

let getSize difficulty =
    match difficulty with
    | Easy -> 4
    | Normal -> 6
    | Hard -> 8

let getRandomTheme () =
    themes[rand.Next(themes.Length)]

let createBoard size =

    let pairCount =
        size * size / 2

    let (themeName, words) =
        getRandomTheme()

    let selectedWords =
        words
        |> List.take pairCount

    let cards =
        selectedWords
        |> List.collect (fun word ->
            [
                {
                    Value = word
                    Theme = themeName
                    Revealed = false
                    Matched = false
                }

                {
                    Value = word
                    Theme = themeName
                    Revealed = false
                    Matched = false
                }
            ])
        |> shuffle

    cards, themeName

let revealCard board index =

    board
    |> List.mapi(fun i card ->

        if i = index then
            { card with Revealed = true }
        else
            card)

let hideCards board i1 i2 =

    board
    |> List.mapi(fun i card ->

        if i = i1 || i = i2 then
            { card with Revealed = false }
        else
            card)

let markMatched board i1 i2 =

    board
    |> List.mapi(fun i card ->

        if i = i1 || i = i2 then
            {
                card with
                    Matched = true
            }
        else
            card)

let isFinished board =
    board |> List.forall(fun c -> c.Matched)