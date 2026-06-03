namespace GUI_Card_Match

open System

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Markup.Xaml
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform

open Types
open Board
open AI
open PersistenceTypes
open Storage
open Themes

// -------------------------------------------------------
// 기댓값 계산
// 보드 크기 n×n (쌍 수 = n*n/2 = p) 에서
// 수학적 기댓값(모든 카드 위치를 모르는 순수 랜덤) 공식:
//   E = p + Σ_{k=1}^{p} (p-k+1)/(총 미매칭 카드 수 - 1)  ← 근사
// 실용적으로는 아래 알려진 근사식 사용:
//   남은 쌍 p개, 남은 카드 2p장일 때
//   E[attempts] ≈ p * (2p-1) / (2p-1)  -- 단순 계산보다
//   정확한 재귀식: E(p) = 1 + (1/(2p-1)) * E(p-1) + ((2p-2)/(2p-1)) * (1 + E(p-1))
//                       = 1 + E(p-1) + (2p-2)/(2p-1)   ... 단, E(1)=1
//   => E(p) = p + Σ_{k=2}^{p} (2k-2)/(2k-1)
// -------------------------------------------------------
module Expected =

    let compute (pairCount : int) : float =
        // E(1) = 1
        // E(p) = E(p-1) + 1 + (2p-2)/(2p-1)   for p >= 2
        let mutable e = 1.0
        for k in 2 .. pairCount do
            e <- e + 1.0 + float (2*k - 2) / float (2*k - 1)
        e

// -------------------------------------------------------

type MainWindow() as this =
    inherit Window()

    // ── 게임 상태 ──
    let mutable isBusy      = false
    let mutable aiRunning   = false
    let mutable gameState   : GameState option = None
    let mutable firstCard   : int option       = None
    let mutable currentDiff : string           = "Easy"

    // ── 이미지 캐시 ──
    let mutable cachedBackImage    : Bitmap option         = None
    let mutable cachedFrontImage   : Bitmap option         = None
    let mutable cachedThemeImages  : Map<string, Bitmap>   = Map.empty

    // ── 영구 데이터 ──
    let mutable saveData : SaveData = Storage.load()

    // ── Collection 현재 선택 테마 ──
    let mutable selectedCollectionTheme : string option = None

    do
        this.InitializeComponent()

        // 난이도 버튼
        this.FindControl<Button>("EasyButton").Click.Add(fun _   -> this.StartGame(4); currentDiff <- "Easy")
        this.FindControl<Button>("NormalButton").Click.Add(fun _ -> this.StartGame(6); currentDiff <- "Normal")
        this.FindControl<Button>("HardButton").Click.Add(fun _   -> this.StartGame(8); currentDiff <- "Hard")

        // 게임 내 버튼
        this.FindControl<Button>("AIplay").Click.Add(fun _       -> this.AIPlay())
        this.FindControl<Button>("NewGameButton").Click.Add(fun _ -> this.ShowDifficulty())

        // Stats
        this.FindControl<Button>("StatsButton").Click.Add(fun _      -> this.ShowStats())
        this.FindControl<Button>("StatsCloseButton").Click.Add(fun _ -> this.HideStats())
        this.FindControl<Button>("StatsResetButton").Click.Add(fun _ ->
            saveData <- Storage.reset()
            this.RebuildStats()
        )

        // Collection
        this.FindControl<Button>("CollectionButton").Click.Add(fun _      -> this.ShowCollection())
        this.FindControl<Button>("CollectionCloseButton").Click.Add(fun _ -> this.HideCollection())
        this.FindControl<Button>("CollectionResetButton").Click.Add(fun _ ->
            saveData <- Storage.reset()
            selectedCollectionTheme <- None
            this.RebuildCollection()
        )

    member private this.InitializeComponent() =
        AvaloniaXamlLoader.Load(this)

    // ===========================================================
    //  화면 전환
    // ===========================================================

    member this.ShowDifficulty() =
        this.FindControl<Grid>("DifficultyPanel").IsVisible <- true
        this.FindControl<Grid>("GamePanel").IsVisible       <- false
        firstCard   <- None
        gameState   <- None
        aiRunning   <- false
        isBusy      <- false

    member this.HideStats() =
        this.FindControl<Grid>("StatsPanel").IsVisible <- false

    member this.HideCollection() =
        this.FindControl<Grid>("CollectionPanel").IsVisible <- false

    // ===========================================================
    //  게임 시작
    // ===========================================================

    member this.StartGame(size : int) =
        this.FindControl<Grid>("DifficultyPanel").IsVisible <- false
        this.FindControl<Grid>("GamePanel").IsVisible       <- true

        let cards, theme = createBoard size

        gameState <-
            Some {
                Board     = cards
                Size      = size
                Attempts  = 0
                Theme     = theme
                SeenCards = Map.empty
            }

        this.FindControl<TextBlock>("themeValue").Text   <- theme
        this.FindControl<TextBlock>("attemptCount").Text <- "0"
        this.FindControl<TextBlock>("GameStatus").Text   <- "Match Cards!"

        firstCard <- None
        isBusy    <- false
        aiRunning <- false
        this.CreateBoard()

    // ===========================================================
    //  보드 렌더링
    // ===========================================================

    member this.CreateBoard() =
        match gameState with
        | None -> ()
        | Some state ->

            let board = this.FindControl<UniformGrid>("BoardGrid")
            board.Children.Clear()
            board.Rows    <- state.Size
            board.Columns <- state.Size

            let backImage =
                match cachedBackImage with
                | Some b -> b
                | None ->
                    let b = Bitmap(AssetLoader.Open(Uri("avares://GUI_Card_Match/Assets/Cards_Back_Small.png")))
                    cachedBackImage <- Some b
                    b

            let frontCardImage =
                match cachedFrontImage with
                | Some b -> b
                | None ->
                    let b = Bitmap(AssetLoader.Open(Uri("avares://GUI_Card_Match/Assets/Cards_Front.png")))
                    cachedFrontImage <- Some b
                    b

            for i in 0 .. state.Board.Length - 1 do
                let card   = state.Board[i]
                let button = Button()

                button.Background       <- Brushes.Transparent
                button.BorderThickness  <- Thickness(0)
                button.Padding          <- Thickness(0)
                button.Margin           <- Thickness(1.0)
                button.HorizontalAlignment <- HorizontalAlignment.Stretch
                button.VerticalAlignment   <- VerticalAlignment.Stretch
                button.Click.Add(fun _ -> this.CardClicked(i))

                if card.Revealed || card.Matched then
                    let layout    = Grid()
                    let cardFront = Image(Source = frontCardImage, Stretch = Stretch.Uniform)
                    layout.Children.Add(cardFront) |> ignore

                    let key = $"{card.Theme}/{card.Value}"
                    let bmp =
                        match cachedThemeImages.TryFind(key) with
                        | Some b -> b
                        | None ->
                            let b = Bitmap(AssetLoader.Open(Uri($"avares://GUI_Card_Match/Assets/Themes/{card.Theme}/{card.Value}.png")))
                            cachedThemeImages <- cachedThemeImages.Add(key, b)
                            b

                    let themeImage = Image(Source = bmp, Stretch = Stretch.Uniform)
                    themeImage.Width  <- 2500
                    themeImage.Height <- 2500
                    themeImage.HorizontalAlignment <- HorizontalAlignment.Center

                    let text = TextBlock()
                    text.Text       <- card.Value
                    text.FontSize   <- 500.0
                    text.HorizontalAlignment <- HorizontalAlignment.Center
                    text.VerticalAlignment   <- VerticalAlignment.Bottom
                    text.Margin     <- Thickness(0.0, 0.0, 0.0, 450.0)
                    text.FontWeight <- FontWeight.SemiBold
                    text.Foreground <- Brushes.Black
                    text.FontFamily <- FontFamily("Comic Sans MS")

                    layout.Children.Add(themeImage) |> ignore
                    layout.Children.Add(text)       |> ignore
                    button.Content <- layout
                else
                    button.Content <- Image(Source = backImage, Stretch = Stretch.Uniform)

                board.Children.Add(button) |> ignore

    // ===========================================================
    //  카드 클릭
    // ===========================================================

    member this.CardClicked(index : int) =
        if isBusy || aiRunning then ()
        else
        match gameState with
        | None -> ()
        | Some state ->
            let selectedCard = state.Board[index]
            if selectedCard.Revealed || selectedCard.Matched then ()
            else

            match firstCard with
            | None ->
                let updatedBoard = revealCard state.Board index
                let card = updatedBoard[index]
                let updatedSeen =
                    match state.SeenCards.TryFind(card.Value) with
                    | Some s -> s.Add(index)
                    | None   -> Set.singleton index

                gameState <-
                    Some { state with
                             Board     = updatedBoard
                             SeenCards = state.SeenCards.Add(card.Value, updatedSeen) }
                firstCard <- Some index
                this.CreateBoard()

            | Some firstIndex ->
                let updatedBoard = revealCard state.Board index
                let card = updatedBoard[index]
                let updatedSeen =
                    match state.SeenCards.TryFind(card.Value) with
                    | Some s -> s.Add(index)
                    | None   -> Set.singleton index

                let updatedState =
                    { state with
                        Board     = updatedBoard
                        Attempts  = state.Attempts + 1
                        SeenCards = state.SeenCards.Add(card.Value, updatedSeen) }

                gameState <- Some updatedState
                this.FindControl<TextBlock>("attemptCount").Text <- updatedState.Attempts.ToString()
                this.CreateBoard()

                let first  = updatedBoard[firstIndex]
                let second = updatedBoard[index]

                if first.Value = second.Value then
                    let matchedBoard = markMatched updatedBoard firstIndex index
                    let finalState   = { updatedState with Board = matchedBoard }
                    gameState <- Some finalState
                    firstCard <- None
                    this.CreateBoard()

                    if isFinished matchedBoard then
                        this.FindControl<TextBlock>("GameStatus").Text <-
                            sprintf "Completed in %d attempts!" finalState.Attempts

                        // ── 기록 저장 ──
                        let record = {
                            Difficulty = currentDiff
                            Theme      = finalState.Theme
                            Attempts   = finalState.Attempts
                            Succeeded  = true
                        }
                        saveData <- Storage.addRecord record saveData
                        Storage.save saveData
                else
                    isBusy <- true
                    async {
                        do! Async.Sleep(1000)
                        let hiddenBoard = hideCards updatedBoard firstIndex index
                        gameState <- Some { updatedState with Board = hiddenBoard }
                        firstCard <- None
                        this.CreateBoard()
                        isBusy    <- false
                        aiRunning <- false
                    }
                    |> Async.StartImmediate

    // ===========================================================
    //  헬퍼
    // ===========================================================

    member this.UpdateAttemptText(attempts : int) =
        this.FindControl<TextBlock>("attemptCount").Text <- attempts.ToString()

    member this.UpdateStatus(text : string) =
        this.FindControl<TextBlock>("GameStatus").Text <- text

    // ===========================================================
    //  AI 플레이 (원본 그대로)
    // ===========================================================

    member this.AIPlay() =
        if isBusy || aiRunning then ()
        else

        isBusy    <- true
        aiRunning <- true

        let rec aiTurn () =
            async {
                if not aiRunning then
                    isBusy <- false
                    return ()
                else

                match gameState with
                | None ->
                    isBusy    <- false
                    aiRunning <- false
                    return ()

                | Some state ->

                    if isFinished state.Board then
                        this.UpdateStatus(sprintf "AI completed in %d attempts!" state.Attempts)

                        // ── AI 완료도 기록 ──
                        let record = {
                            Difficulty = currentDiff
                            Theme      = state.Theme
                            Attempts   = state.Attempts
                            Succeeded  = true
                        }
                        saveData <- Storage.addRecord record saveData
                        Storage.save saveData

                        isBusy    <- false
                        aiRunning <- false
                        return ()
                    else

                    match firstCard with
                    | Some fIdx ->

                        let fCard = state.Board[fIdx]
                        let knownSecond =
                            match state.SeenCards.TryFind(fCard.Value) with
                            | Some indices ->
                                indices |> Set.toList |> List.tryFind (fun i -> i <> fIdx && not state.Board[i].Matched)
                            | None -> None

                        let secondOpt =
                            match knownSecond with
                            | Some i -> Some i
                            | None ->
                                state.Board
                                |> List.indexed
                                |> List.tryFind (fun (i, c) -> i <> fIdx && not c.Matched && not c.Revealed)
                                |> Option.map fst

                        match secondOpt with
                        | None ->
                            firstCard <- None
                            return! aiTurn()

                        | Some sIdx ->
                            let board2 = revealCard state.Board sIdx
                            let sCard  = board2[sIdx]
                            let updatedSeen =
                                match state.SeenCards.TryFind(sCard.Value) with
                                | Some s -> s.Add(sIdx)
                                | None   -> Set.singleton sIdx

                            let state2 =
                                { state with
                                    Board     = board2
                                    Attempts  = state.Attempts + 1
                                    SeenCards = state.SeenCards.Add(sCard.Value, updatedSeen) }

                            gameState <- Some state2
                            this.UpdateAttemptText(state2.Attempts)
                            this.CreateBoard()
                            do! Async.Sleep(800)
                            if not aiRunning then return ()

                            if fCard.Value = sCard.Value then
                                let matched = markMatched board2 fIdx sIdx
                                gameState   <- Some { state2 with Board = matched }
                                firstCard   <- None
                                this.CreateBoard()
                                do! Async.Sleep(400)
                                if not aiRunning then return ()
                                return! aiTurn()
                            else
                                let hidden  = hideCards board2 fIdx sIdx
                                gameState   <- Some { state2 with Board = hidden }
                                firstCard   <- None
                                this.CreateBoard()
                                do! Async.Sleep(400)
                                if not aiRunning then return ()
                                return! aiTurn()

                    | None ->

                        let knownPair =
                            state.SeenCards
                            |> Map.tryPick (fun _ indices ->
                                let valid =
                                    indices |> Set.toList |> List.filter (fun i -> not state.Board[i].Matched)
                                match valid with
                                | a :: b :: _ -> Some(a, b)
                                | _           -> None)

                        match knownPair with
                        | Some(i1, i2) ->
                            let board1 = revealCard state.Board i1
                            gameState  <- Some { state with Board = board1 }
                            this.CreateBoard()
                            do! Async.Sleep(600)
                            if not aiRunning then return ()

                            let board2 = revealCard board1 i2
                            let state2 = { state with Board = board2; Attempts = state.Attempts + 1 }
                            gameState <- Some state2
                            this.UpdateAttemptText(state2.Attempts)
                            this.CreateBoard()
                            do! Async.Sleep(800)
                            if not aiRunning then return ()

                            let matched = markMatched board2 i1 i2
                            gameState   <- Some { state2 with Board = matched }
                            this.CreateBoard()
                            do! Async.Sleep(400)
                            if not aiRunning then return ()
                            return! aiTurn()

                        | None ->

                            let unknownOpt =
                                state.Board
                                |> List.indexed
                                |> List.tryFind (fun (i, c) ->
                                    not c.Matched
                                    && not (state.SeenCards |> Map.exists (fun _ s -> Set.contains i s)))
                                |> Option.map fst

                            match unknownOpt with
                            | None ->
                                isBusy    <- false
                                aiRunning <- false
                                return ()

                            | Some fIdx ->
                                let board1 = revealCard state.Board fIdx
                                let fCard  = board1[fIdx]
                                let updatedSeen1 =
                                    match state.SeenCards.TryFind(fCard.Value) with
                                    | Some s -> s.Add(fIdx)
                                    | None   -> Set.singleton fIdx

                                let state1 =
                                    { state with
                                        Board     = board1
                                        SeenCards = state.SeenCards.Add(fCard.Value, updatedSeen1) }

                                gameState <- Some state1
                                this.CreateBoard()
                                do! Async.Sleep(600)
                                if not aiRunning then return ()

                                let knownSecond =
                                    match state1.SeenCards.TryFind(fCard.Value) with
                                    | Some indices ->
                                        indices |> Set.toList |> List.tryFind (fun i -> i <> fIdx && not state1.Board[i].Matched)
                                    | None -> None

                                match knownSecond with
                                | Some sIdx ->
                                    let board2 = revealCard board1 sIdx
                                    let state2 = { state1 with Board = board2; Attempts = state.Attempts + 1 }
                                    gameState <- Some state2
                                    this.UpdateAttemptText(state2.Attempts)
                                    this.CreateBoard()
                                    do! Async.Sleep(800)
                                    if not aiRunning then return ()

                                    let matched = markMatched board2 fIdx sIdx
                                    gameState   <- Some { state2 with Board = matched }
                                    this.CreateBoard()
                                    do! Async.Sleep(400)
                                    if not aiRunning then return ()
                                    return! aiTurn()

                                | None ->
                                    let unknown2Opt =
                                        state1.Board
                                        |> List.indexed
                                        |> List.tryFind (fun (i, c) ->
                                            i <> fIdx
                                            && not c.Matched
                                            && not (state1.SeenCards |> Map.exists (fun _ s -> Set.contains i s)))
                                        |> Option.map fst

                                    match unknown2Opt with
                                    | None ->
                                        isBusy    <- false
                                        aiRunning <- false
                                        return ()

                                    | Some sIdx ->
                                        let board2 = revealCard board1 sIdx
                                        let sCard  = board2[sIdx]
                                        let updatedSeen2 =
                                            match state1.SeenCards.TryFind(sCard.Value) with
                                            | Some s -> s.Add(sIdx)
                                            | None   -> Set.singleton sIdx

                                        let state2 =
                                            { state1 with
                                                Board     = board2
                                                Attempts  = state.Attempts + 1
                                                SeenCards = state1.SeenCards.Add(sCard.Value, updatedSeen2) }

                                        gameState <- Some state2
                                        this.UpdateAttemptText(state2.Attempts)
                                        this.CreateBoard()
                                        do! Async.Sleep(800)
                                        if not aiRunning then return ()

                                        if fCard.Value = sCard.Value then
                                            let matched = markMatched board2 fIdx sIdx
                                            gameState   <- Some { state2 with Board = matched }
                                            this.CreateBoard()
                                        else
                                            let hidden  = hideCards board2 fIdx sIdx
                                            gameState   <- Some { state2 with Board = hidden }
                                            this.CreateBoard()

                                        do! Async.Sleep(400)
                                        if not aiRunning then return ()
                                        return! aiTurn()
            }

        Async.StartImmediate(aiTurn())

    // ===========================================================
    //  STATS 패널
    // ===========================================================

    member this.ShowStats() =
        this.RebuildStats()
        this.FindControl<Grid>("StatsPanel").IsVisible <- true

    member this.RebuildStats() =
        let container = this.FindControl<StackPanel>("StatsContent")
        container.Children.Clear()

        let difficulties = ["Easy", 4*4/2; "Normal", 6*6/2; "Hard", 8*8/2]

        for (diff, pairCount) in difficulties do

            let records =
                saveData.PlayHistory
                |> List.filter (fun r -> r.Difficulty = diff && r.Succeeded)

            // ── 난이도 헤더 ──
            let header = TextBlock()
            header.Text       <- sprintf "━━━  %s  ━━━" diff
            header.FontSize   <- 20.0
            header.FontWeight <- FontWeight.Bold
            header.Foreground <-
                match diff with
                | "Easy"   -> SolidColorBrush(Color.Parse("#2ecc71"))
                | "Normal" -> SolidColorBrush(Color.Parse("#f1c40f"))
                | _        -> SolidColorBrush(Color.Parse("#e74c3c"))
            header.HorizontalAlignment <- HorizontalAlignment.Center
            header.Margin <- Thickness(0.0, 4.0, 0.0, 6.0)
            container.Children.Add(header) |> ignore

            if records.IsEmpty then
                let noData = TextBlock()
                noData.Text       <- "  No records yet."
                noData.Foreground <- SolidColorBrush(Color.Parse("#888888"))
                noData.FontSize   <- 14.0
                noData.Margin     <- Thickness(4.0, 0.0, 0.0, 8.0)
                container.Children.Add(noData) |> ignore
            else
                // 개별 기록
                for (idx, r) in records |> List.indexed do
                    let row = Grid()
                    row.ColumnDefinitions <-
                        ColumnDefinitions("30,160,80,*")
                    row.Margin <- Thickness(4.0, 2.0, 4.0, 2.0)

                    let mkTB (col:int) (txt:string) (fg:string) =
                        let tb = TextBlock()
                        tb.Text       <- txt
                        tb.FontSize   <- 13.0
                        tb.Foreground <- SolidColorBrush(Color.Parse(fg))
                        tb.VerticalAlignment <- VerticalAlignment.Center
                        Grid.SetColumn(tb, col)
                        tb

                    row.Children.Add(mkTB 0 (sprintf "#%d" (idx+1)) "#888888") |> ignore
                    row.Children.Add(mkTB 1 (sprintf "🎨 %s" r.Theme)  "#dddddd") |> ignore
                    row.Children.Add(mkTB 2 (sprintf "%d tries" r.Attempts) "#FFD93D") |> ignore
                    row.Children.Add(mkTB 3 "✅" "#2ecc71") |> ignore
                    container.Children.Add(row) |> ignore

                // ── 요약 ──
                let successCount = records.Length
                let avgAttempts  = records |> List.averageBy (fun r -> float r.Attempts)
                let mathExpected = Expected.compute pairCount
                let dataExpected = avgAttempts   // 내 플레이 평균 = 데이터 기반 기댓값

                let sep = Border()
                sep.Height     <- 1.0
                sep.Background <- SolidColorBrush(Color.Parse("#444444"))
                sep.Margin     <- Thickness(0.0, 6.0, 0.0, 6.0)
                container.Children.Add(sep) |> ignore

                let summaryPanel = StackPanel()
                summaryPanel.Spacing <- 4.0
                summaryPanel.Margin  <- Thickness(8.0, 0.0, 0.0, 8.0)

                let mkSummary (label:string) (value:string) (valueColor:string) =
                    let sp = StackPanel()
                    sp.Orientation <- Orientation.Horizontal
                    sp.Spacing     <- 6.0
                    let lbl = TextBlock(Text = label, FontSize = 14.0, Foreground = SolidColorBrush(Color.Parse("#aaaaaa")))
                    let val_ = TextBlock(Text = value, FontSize = 14.0, FontWeight = FontWeight.SemiBold, Foreground = SolidColorBrush(Color.Parse(valueColor)))
                    sp.Children.Add(lbl)  |> ignore
                    sp.Children.Add(val_) |> ignore
                    sp

                summaryPanel.Children.Add(mkSummary "Total games:" (sprintf "%d" successCount) "#ffffff") |> ignore
                summaryPanel.Children.Add(mkSummary "Avg attempts (my plays):" (sprintf "%.2f" dataExpected) "#44B1F0") |> ignore
                summaryPanel.Children.Add(mkSummary "Expected attempts (random theory):" (sprintf "%.2f" mathExpected) "#9b59b6") |> ignore
                container.Children.Add(summaryPanel) |> ignore

    // ===========================================================
    //  COLLECTION 패널
    // ===========================================================

    member this.ShowCollection() =
        this.RebuildCollection()
        this.FindControl<Grid>("CollectionPanel").IsVisible <- true

    member this.RebuildCollection() =
        let tabBar = this.FindControl<StackPanel>("ThemeTabBar")
        tabBar.Children.Clear()

        let allThemeNames =
            themes |> List.map fst

        let discovered = saveData.DiscoveredThemes

        // 선택 테마가 없으면 첫 번째 발견 테마(또는 첫 테마)로 초기화
        if selectedCollectionTheme.IsNone then
            selectedCollectionTheme <-
                match discovered with
                | h :: _ -> Some h
                | []     -> Some allThemeNames[0]

        // 테마 탭 버튼 생성
        for name in allThemeNames do
            let isDiscovered = List.contains name discovered
            let isSelected   = selectedCollectionTheme = Some name

            let btn = Button()
            btn.Content    <- if isDiscovered then name else "???"
            btn.FontWeight <- FontWeight.Bold
            btn.FontSize   <- 13.0
            btn.Padding    <- Thickness(10.0, 6.0)
            btn.Margin     <- Thickness(0.0, 0.0, 6.0, 0.0)

            btn.Background <-
                if isSelected && isDiscovered then
                    SolidColorBrush(Color.Parse("#2980b9")) :> IBrush
                elif isDiscovered then
                    SolidColorBrush(Color.Parse("#2c3e50")) :> IBrush
                else
                    SolidColorBrush(Color.Parse("#1a1a1a")) :> IBrush

            btn.Foreground <-
                if isDiscovered then
                    SolidColorBrush(Color.Parse("#ffffff")) :> IBrush
                else
                    SolidColorBrush(Color.Parse("#555555")) :> IBrush

            if isDiscovered then
                btn.Click.Add(fun _ ->
                    selectedCollectionTheme <- Some name
                    this.RebuildCollection()
                )
            else
                btn.IsEnabled <- false

            tabBar.Children.Add(btn) |> ignore

        // 카드 그리드 채우기
        this.RebuildCollectionGrid()

    member this.RebuildCollectionGrid() =
        let grid = this.FindControl<UniformGrid>("CollectionGrid")
        grid.Children.Clear()

        let frontCardImage =
            match cachedFrontImage with
            | Some b -> b
            | None ->
                let b = Bitmap(AssetLoader.Open(Uri("avares://GUI_Card_Match/Assets/Cards_Front.png")))
                cachedFrontImage <- Some b
                b

        match selectedCollectionTheme with
        | None -> ()
        | Some themeName ->

            let discovered = saveData.DiscoveredThemes
            let isDiscovered = List.contains themeName discovered

            if not isDiscovered then
                // 미발견 → 잠금 카드 32장
                for _ in 1 .. 32 do
                    let border = Border()
                    border.Width        <- 80.0
                    border.Height       <- 110.0
                    border.Margin       <- Thickness(4.0)
                    border.CornerRadius <- CornerRadius(6.0)
                    border.Background   <- SolidColorBrush(Color.Parse("#1a1a1a"))

                    let tb = TextBlock()
                    tb.Text                  <- "🔒"
                    tb.FontSize              <- 28.0
                    tb.HorizontalAlignment   <- HorizontalAlignment.Center
                    tb.VerticalAlignment     <- VerticalAlignment.Center
                    border.Child <- tb

                    grid.Children.Add(border) |> ignore
            else
                // 발견됨 → 해당 테마 단어 목록 찾기
                let wordsOpt =
                    themes
                    |> List.tryFind (fun (name, _) -> name = themeName)
                    |> Option.map snd

                match wordsOpt with
                | None -> ()
                | Some words ->
                    // 32장 = 16쌍. 단어가 16개 미만이면 있는 만큼만, 많으면 앞 16개
                    let cardWords =
                        words |> List.truncate 16

                    // 각 단어마다 카드 2장 (쌍)
                    for word in cardWords do
                        for _ in 1 .. 2 do
                            let layout = Grid()
                            layout.Width  <- 80.0
                            layout.Height <- 110.0
                            layout.Margin <- Thickness(4.0)

                            let bg = Image(Source = frontCardImage, Stretch = Stretch.Fill)
                            layout.Children.Add(bg) |> ignore

                            let key = $"{themeName}/{word}"
                            let bmpOpt =
                                try
                                    let bmp =
                                        match cachedThemeImages.TryFind(key) with
                                        | Some b -> b
                                        | None ->
                                            let b = Bitmap(AssetLoader.Open(Uri($"avares://GUI_Card_Match/Assets/Themes/{themeName}/{word}.png")))
                                            cachedThemeImages <- cachedThemeImages.Add(key, b)
                                            b
                                    Some bmp
                                with _ -> None

                            match bmpOpt with
                            | Some bmp ->
                                let img = Image(Source = bmp, Stretch = Stretch.Uniform)
                                img.Width  <- 60.0
                                img.Height <- 60.0
                                img.HorizontalAlignment <- HorizontalAlignment.Center
                                img.VerticalAlignment   <- VerticalAlignment.Center
                                img.Margin <- Thickness(0.0, 8.0, 0.0, 0.0)
                                layout.Children.Add(img) |> ignore
                            | None -> ()

                            let tb = TextBlock()
                            tb.Text                <- word
                            tb.FontSize            <- 9.0
                            tb.FontWeight          <- FontWeight.SemiBold
                            tb.HorizontalAlignment <- HorizontalAlignment.Center
                            tb.VerticalAlignment   <- VerticalAlignment.Bottom
                            tb.Margin              <- Thickness(0.0, 0.0, 0.0, 6.0)
                            tb.Foreground          <- Brushes.Black
                            tb.FontFamily          <- FontFamily("Comic Sans MS")
                            layout.Children.Add(tb) |> ignore

                            grid.Children.Add(layout) |> ignore