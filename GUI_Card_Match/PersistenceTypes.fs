module PersistenceTypes

type PlayRecord =
    {
        Difficulty  : string   // "Easy" | "Normal" | "Hard"
        Theme       : string
        Attempts    : int
        Succeeded   : bool     // true = 완료, false = 미완료(포기 등 – 현재는 항상 true)
    }

type SaveData =
    {
        PlayHistory         : PlayRecord list
        DiscoveredThemes    : string list      // 한 번이라도 플레이된 테마 이름 목록
    }

    static member Empty =
        {
            PlayHistory      = []
            DiscoveredThemes = []
        }