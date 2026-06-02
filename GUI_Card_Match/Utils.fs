module Utils

open System

let rand = Random()

let shuffle list =
    list
    |> List.sortBy (fun _ -> rand.Next())