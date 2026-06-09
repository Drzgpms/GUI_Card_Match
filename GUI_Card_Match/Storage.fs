module Storage

open System
open System.IO
open System.Text.Json
open PersistenceTypes

let private saveDir =
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData),
        "CardMatchGame"
    )

let private savePath = Path.Combine(saveDir, "save.json")

let private jsonOptions =
    let o = JsonSerializerOptions()
    o.WriteIndented <- true
    o

let load () : SaveData =
    try
        if File.Exists(savePath) then
            let json = File.ReadAllText(savePath)
            JsonSerializer.Deserialize<SaveData>(json, jsonOptions)
        else
            SaveData.Empty
    with _ ->
        SaveData.Empty

let save (data : SaveData) : unit =
    try
        Directory.CreateDirectory(saveDir) |> ignore
        let json = JsonSerializer.Serialize(data, jsonOptions)
        File.WriteAllText(savePath, json)
    with _ -> ()

// 플레이 기록 추가 + 테마 발견 기록
let addRecord (record : PlayRecord) (data : SaveData) : SaveData =
    let themes =
        if List.contains record.Theme data.DiscoveredThemes then
            data.DiscoveredThemes
        else
            data.DiscoveredThemes @ [record.Theme]
    { data with
        PlayHistory      = data.PlayHistory @ [record]
        DiscoveredThemes = themes }

// 본 카드 누적 저장
let addSeenCards (theme : string) (cardValues : string list) (data : SaveData) : SaveData =
    let existing =
        match data.SeenCardsByTheme.TryFind(theme) with
        | Some lst -> lst
        | None     -> []
    let merged = (existing @ cardValues) |> List.distinct |> List.sort
    { data with SeenCardsByTheme = data.SeenCardsByTheme.Add(theme, merged) }

// 스탯만 리셋 (컬렉션 유지)
let resetStats (data : SaveData) : SaveData =
    let newData = { data with PlayHistory = [] }
    save newData
    newData

// 컬렉션만 리셋 (스탯 유지)
let resetCollection (data : SaveData) : SaveData =
    let newData = { data with DiscoveredThemes = []; SeenCardsByTheme = Map.empty }
    save newData
    newData