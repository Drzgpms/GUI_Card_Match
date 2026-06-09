module PersistenceTypes

type PlayRecord =
    {
        Difficulty  : string   // "Easy" | "Normal" | "Hard"
        Theme       : string
        Attempts    : int
        Succeeded   : bool     // true = 클리어, false = 포기
    }

type SaveData =
    {
        PlayHistory      : PlayRecord list
        DiscoveredThemes : string list              // 플레이된 테마 이름 목록
        SeenCardsByTheme : Map<string, string list> // 테마 → 본 카드 이름 목록
    }

    static member Empty =
        {
            PlayHistory      = []
            DiscoveredThemes = []
            SeenCardsByTheme = Map.empty
        }