module Storage

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open PersistenceTypes

// ---------- 저장 경로 ----------
// Windows: %APPDATA%\CardMatchGame\save.json
// macOS/Linux: ~/.config/CardMatchGame/save.json
let private saveDir =
    Path.Combine(
        Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData),
        "CardMatchGame"
    )

let private savePath =
    Path.Combine(saveDir, "save.json")

// ---------- JSON 옵션 ----------
let private jsonOptions =
    let o = JsonSerializerOptions()
    o.WriteIndented <- true
    o

// ---------- 공개 API ----------

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
    with _ ->
        ()   // 저장 실패해도 게임은 계속

let addRecord (record : PlayRecord) (data : SaveData) : SaveData =
    let themes =
        if List.contains record.Theme data.DiscoveredThemes then
            data.DiscoveredThemes
        else
            data.DiscoveredThemes @ [record.Theme]

    { data with
        PlayHistory      = data.PlayHistory @ [record]
        DiscoveredThemes = themes }

let reset () : SaveData =
    let empty = SaveData.Empty
    save empty
    empty