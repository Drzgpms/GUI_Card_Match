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

// ── 기댓값 계산 ──
// E(1) = 1,  E(p) = E(p-1) + 1 + (2p-2)/(2p-1)
module Expected =
    let compute (pairCount : int) : float =
        let mutable e = 1.0
        for k in 2 .. pairCount do
            e <- e + 1.0 + float (2*k - 2) / float (2*k - 1)
        e

type MainWindow() as this =
    inherit Window()

    // ── 게임 상태 ──
    let mutable isBusy      = false
    let mutable aiRunning   = false
    let mutable gameState   : GameState option = None
    let mutable firstCard   : int option       = None
    let mutable currentDiff : string           = "Easy"

    // ── 이미지 캐시 ──
    let mutable cachedBackImage   : Bitmap option       = None
    let mutable cachedFrontImage  : Bitmap option       = None
    let mutable cachedThemeImages : Map<string, Bitmap> = Map.empty

    // ── 영구 데이터 ──
    let mutable saveData : SaveData = Storage.load()

    // ── 컬렉션 선택 테마 & ComboBox 핸들러 등록 여부 ──
    let mutable selectedCollectionTheme : string option = None
    let mutable comboHandlerAttached                    = false

    do
        this.InitializeComponent()

        // 난이도 버튼 (currentDiff를 먼저 설정 후 StartGame)
        this.FindControl<Button>("EasyButton").Click.Add(fun _ ->
            currentDiff <- "Easy"; this.StartGame(4))
        this.FindControl<Button>("NormalButton").Click.Add(fun _ ->
            currentDiff <- "Normal"; this.StartGame(6))
        this.FindControl<Button>("HardButton").Click.Add(fun _ ->
            currentDiff <- "Hard"; this.StartGame(8))

        // 게임 내 버튼
        this.FindControl<Button>("AIplay").Click.Add(fun _        -> this.AIPlay())
        this.FindControl<Button>("NewGameButton").Click.Add(fun _ -> this.ShowDifficulty())

        // Stats
        this.FindControl<Button>("StatsButton").Click.Add(fun _      -> this.ShowStats())
        this.FindControl<Button>("StatsCloseButton").Click.Add(fun _ -> this.HideStats())
        this.FindControl<Button>("StatsResetButton").Click.Add(fun _ ->
            saveData <- Storage.resetStats saveData
            this.RebuildStats())

        // Collection
        this.FindControl<Button>("CollectionButton").Click.Add(fun _      -> this.ShowCollection())
        this.FindControl<Button>("CollectionCloseButton").Click.Add(fun _ -> this.HideCollection())
        this.FindControl<Button>("CollectionResetButton").Click.Add(fun _ ->
            saveData <- Storage.resetCollection saveData
            selectedCollectionTheme <- None
            this.RebuildCollection())

    member private this.InitializeComponent() =
        AvaloniaXamlLoader.Load(this)

    // ── 헬퍼: 현재 게임에서 본 카드 값 목록 추출 ──
    member private this.ExtractSeenValues(state : GameState) : string list =
        state.SeenCards |> Map.toList |> List.map fst

    // ── 헬퍼: 게임 기록 저장 (플레이어 전용, AI 호출 금지) ──
    member private this.SaveRecord(state : GameState, succeeded : bool) =
        let record = {
            Difficulty = currentDiff
            Theme      = state.Theme
            Attempts   = state.Attempts
            Succeeded  = succeeded
        }
        saveData <- Storage.addRecord record saveData
        let seenValues = this.ExtractSeenValues(state)
        saveData <- Storage.addSeenCards state.Theme seenValues saveData
        Storage.save saveData

    // ===========================================================
    //  화면 전환
    // ===========================================================

    member this.ShowDifficulty() =
        // AI 플레이 중이 아닐 때만 기록
        if not aiRunning then
            match gameState with
            | Some state when not (isFinished state.Board) && state.Attempts > 0 ->
                // 미클리어 포기 = 실패 기록
                this.SaveRecord(state, false)
            | _ -> ()

        this.FindControl<Grid>("DifficultyPanel").IsVisible <- true
        this.FindControl<Grid>("GamePanel").IsVisible       <- false
        firstCard <- None
        gameState <- None
        aiRunning <- false
        isBusy    <- false

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

                button.Background      <- Brushes.Transparent
                button.BorderThickness <- Thickness(0)
                button.Padding         <- Thickness(0)
                button.Margin          <- Thickness(1.0)
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
    //  카드 클릭 (플레이어)
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
                let card         = updatedBoard[index]
                let updatedSeen  =
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
                let card         = updatedBoard[index]
                let updatedSeen  =
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
                        // ── 클리어 기록 (플레이어) ──
                        this.SaveRecord(finalState, true)
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
    //  AI 플레이 (기록 없음)
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
                        // AI 플레이는 스탯/컬렉션 기록 없음
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
                                indices |> Set.toList
                                |> List.tryFind (fun i -> i <> fIdx && not state.Board[i].Matched)
                            | None -> None

                        let secondOpt =
                            match knownSecond with
                            | Some i -> Some i
                            | None ->
                                state.Board
                                |> List.indexed
                                |> List.tryFind (fun (i, c) ->
                                    i <> fIdx && not c.Matched && not c.Revealed)
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
                                    indices |> Set.toList
                                    |> List.filter (fun i -> not state.Board[i].Matched)
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
                                    && not (state.SeenCards
                                            |> Map.exists (fun _ s -> Set.contains i s)))
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
                                        indices |> Set.toList
                                        |> List.tryFind (fun i ->
                                            i <> fIdx && not state1.Board[i].Matched)
                                    | None -> None

                                match knownSecond with
                                | Some sIdx ->
                                    let board2 = revealCard board1 sIdx
                                    let state2 =
                                        { state1 with
                                            Board    = board2
                                            Attempts = state.Attempts + 1 }
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
                                            && not (state1.SeenCards
                                                    |> Map.exists (fun _ s -> Set.contains i s)))
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

            let allRecords     = saveData.PlayHistory |> List.filter (fun r -> r.Difficulty = diff)
            let successRecords = allRecords           |> List.filter (fun r -> r.Succeeded)
            let failRecords    = allRecords           |> List.filter (fun r -> not r.Succeeded)

            // 헤더
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

            if allRecords.IsEmpty then
                let noData = TextBlock()
                noData.Text       <- "  No records yet."
                noData.Foreground <- SolidColorBrush(Color.Parse("#888888"))
                noData.FontSize   <- 14.0
                noData.Margin     <- Thickness(4.0, 0.0, 0.0, 8.0)
                container.Children.Add(noData) |> ignore
            else
                let mkTB (col:int) (txt:string) (fg:string) =
                    let tb = TextBlock()
                    tb.Text       <- txt
                    tb.FontSize   <- 13.0
                    tb.Foreground <- SolidColorBrush(Color.Parse(fg))
                    tb.VerticalAlignment <- VerticalAlignment.Center
                    Grid.SetColumn(tb, col)
                    tb

                // 성공 기록
                for (idx, r) in successRecords |> List.indexed do
                    let row = Grid()
                    row.ColumnDefinitions <- ColumnDefinitions("30,160,80,*")
                    row.Margin <- Thickness(4.0, 2.0, 4.0, 2.0)
                    row.Children.Add(mkTB 0 (sprintf "#%d" (idx+1))       "#888888") |> ignore
                    row.Children.Add(mkTB 1 (sprintf "🎨 %s" r.Theme)     "#dddddd") |> ignore
                    row.Children.Add(mkTB 2 (sprintf "%d tries" r.Attempts) "#FFD93D") |> ignore
                    row.Children.Add(mkTB 3 "✅" "#2ecc71") |> ignore
                    container.Children.Add(row) |> ignore

                // 실패 기록
                for (idx, r) in failRecords |> List.indexed do
                    let row = Grid()
                    row.ColumnDefinitions <- ColumnDefinitions("30,160,80,*")
                    row.Margin <- Thickness(4.0, 2.0, 4.0, 2.0)
                    row.Children.Add(mkTB 0 (sprintf "#%d" (successRecords.Length + idx + 1)) "#888888") |> ignore
                    row.Children.Add(mkTB 1 (sprintf "🎨 %s" r.Theme)     "#dddddd") |> ignore
                    row.Children.Add(mkTB 2 (sprintf "%d tries" r.Attempts) "#FFD93D") |> ignore
                    row.Children.Add(mkTB 3 "❌" "#e74c3c") |> ignore
                    container.Children.Add(row) |> ignore

                // 요약
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
                    let lbl  = TextBlock(Text = label, FontSize = 14.0,
                                         Foreground = SolidColorBrush(Color.Parse("#aaaaaa")))
                    let val_ = TextBlock(Text = value, FontSize = 14.0,
                                         FontWeight = FontWeight.SemiBold,
                                         Foreground = SolidColorBrush(Color.Parse(valueColor)))
                    sp.Children.Add(lbl)  |> ignore
                    sp.Children.Add(val_) |> ignore
                    sp

                summaryPanel.Children.Add(
                    mkSummary "Total games:" (sprintf "%d ✅%d ❌%d"
                        allRecords.Length successRecords.Length failRecords.Length) "#ffffff") |> ignore

                if not successRecords.IsEmpty then
                    let avg         = successRecords |> List.averageBy (fun r -> float r.Attempts)
                    let mathExpected = Expected.compute pairCount
                    summaryPanel.Children.Add(
                        mkSummary "Avg attempts (clears):" (sprintf "%.2f" avg) "#44B1F0") |> ignore
                    summaryPanel.Children.Add(
                        mkSummary "Expected (random theory):" (sprintf "%.2f" mathExpected) "#9b59b6") |> ignore

                container.Children.Add(summaryPanel) |> ignore

    // ===========================================================
    //  COLLECTION 패널
    // ===========================================================

    member this.ShowCollection() =
        this.RebuildCollection()
        this.FindControl<Grid>("CollectionPanel").IsVisible <- true

    member this.RebuildCollection() =
        let combo         = this.FindControl<ComboBox>("ThemeComboBox")
        let allThemeNames = themes |> List.map fst
        let discovered    = saveData.DiscoveredThemes

        // 선택 테마 초기화
        if selectedCollectionTheme.IsNone then
            selectedCollectionTheme <-
                match discovered with
                | h :: _ -> Some h
                | []     -> None

        // ComboBox 이벤트 최초 1회만 등록
        if not comboHandlerAttached then
            combo.SelectionChanged.Add(fun _ ->
                let names = themes |> List.map fst
                let disc  = saveData.DiscoveredThemes
                let i     = combo.SelectedIndex
                if i >= 0 && i < names.Length then
                    let name = names[i]
                    if List.contains name disc then
                        selectedCollectionTheme <- Some name
                        this.RebuildCollectionGrid()
            )
            comboHandlerAttached <- true

        // 아이템 목록 업데이트
        let items =
            allThemeNames
            |> List.map (fun name ->
                if List.contains name discovered then name else "???")
            |> List.toArray

        combo.ItemsSource <- items

        // 선택 인덱스 설정
        let selIdx =
            match selectedCollectionTheme with
            | Some sel -> allThemeNames |> List.tryFindIndex (fun n -> n = sel) |> Option.defaultValue -1
            | None     -> -1
        combo.SelectedIndex <- selIdx

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

        // 잠금 카드
        let makeLockCard () =
            let border = Border()
            border.Width        <- 80.0
            border.Height       <- 110.0
            border.Margin       <- Thickness(4.0)
            border.CornerRadius <- CornerRadius(6.0)
            border.Background   <- SolidColorBrush(Color.Parse("#1a1a1a"))
            let tb = TextBlock()
            tb.Text                <- "🔒"
            tb.FontSize            <- 28.0
            tb.HorizontalAlignment <- HorizontalAlignment.Center
            tb.VerticalAlignment   <- VerticalAlignment.Center
            border.Child <- tb
            border :> Avalonia.Controls.Control

        // 공개 카드
        let makeCard (themeName: string) (word: string) =
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
                            let b = Bitmap(AssetLoader.Open(
                                        Uri($"avares://GUI_Card_Match/Assets/Themes/{themeName}/{word}.png")))
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
            layout :> Avalonia.Controls.Control

        match selectedCollectionTheme with
        | None ->
            // 발견된 테마 없음
            for _ in 1 .. 32 do
                grid.Children.Add(makeLockCard()) |> ignore

        | Some themeName ->
            let discovered   = saveData.DiscoveredThemes
            let isDiscovered = List.contains themeName discovered

            if not isDiscovered then
                for _ in 1 .. 32 do
                    grid.Children.Add(makeLockCard()) |> ignore
            else
                let allWordsOpt =
                    themes
                    |> List.tryFind (fun (name, _) -> name = themeName)
                    |> Option.map snd

                match allWordsOpt with
                | None -> ()
                | Some allWords ->
                    // 본 카드 목록
                    let seenValues =
                        match saveData.SeenCardsByTheme.TryFind(themeName) with
                        | Some lst -> lst
                        | None     -> []

                    // 전체 32종을 순서대로 1장씩 (쌍 없음)
                    // 본 카드 → 공개, 미발견 → 잠금
                    for word in allWords do
                        if List.contains word seenValues then
                            grid.Children.Add(makeCard themeName word) |> ignore
                        else
                            grid.Children.Add(makeLockCard()) |> ignore