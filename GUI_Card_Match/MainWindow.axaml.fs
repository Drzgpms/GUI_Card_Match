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

type MainWindow() as this =
    inherit Window()

    let mutable isBusy = false

    let mutable aiRunning = false

    let mutable gameState : GameState option = None

    let mutable firstCard : int option = None

    let mutable cachedBackImage : Bitmap option = None

    let mutable cachedFrontImage : Bitmap option = None

    let mutable cachedThemeImages : Map<string, Bitmap> = Map.empty

    do
        this.InitializeComponent()

        let easy =
            this.FindControl<Button>("EasyButton")

        let normal =
            this.FindControl<Button>("NormalButton")

        let hard =
            this.FindControl<Button>("HardButton")

        let aiButton =
            this.FindControl<Button>("AIplay")

        let newGame =
            this.FindControl<Button>("NewGameButton")

        easy.Click.Add(fun _ -> this.StartGame(4))
        normal.Click.Add(fun _ -> this.StartGame(6))
        hard.Click.Add(fun _ -> this.StartGame(8))

        aiButton.Click.Add(fun _ -> this.AIPlay())

        newGame.Click.Add(fun _ -> this.ShowDifficulty())

    member private this.InitializeComponent() =
        AvaloniaXamlLoader.Load(this)

    member this.ShowDifficulty() =

        let difficultyPanel =
            this.FindControl<StackPanel>("DifficultyPanel")

        let gamePanel =
            this.FindControl<Grid>("GamePanel")

        difficultyPanel.IsVisible <- true
        gamePanel.IsVisible <- false

        firstCard <- None
        gameState <- None
        aiRunning <- false
        isBusy <- false

    member this.StartGame(size : int) =

        let difficultyPanel =
            this.FindControl<StackPanel>("DifficultyPanel")

        let gamePanel =
            this.FindControl<Grid>("GamePanel")

        difficultyPanel.IsVisible <- false
        gamePanel.IsVisible <- true

        let cards, theme =
            createBoard size

        gameState <-
            Some
                {
                    Board = cards
                    Size = size
                    Attempts = 0
                    Theme = theme
                    SeenCards = Map.empty
                }

        let themeValue =
            this.FindControl<TextBlock>("themeValue")

        themeValue.Text <- theme

        let attemptCount =
            this.FindControl<TextBlock>("attemptCount")

        attemptCount.Text <- "0"

        let status =
            this.FindControl<TextBlock>("GameStatus")

        status.Text <- "Match Cards!"

        firstCard <- None
        isBusy <- false
        aiRunning <- false
        this.CreateBoard()

    member this.CreateBoard() =

        match gameState with

        | None ->
            ()

        | Some state ->

            let board =
                this.FindControl<UniformGrid>("BoardGrid")

            board.Children.Clear()

            board.Rows <- state.Size
            board.Columns <- state.Size

            let backImage =
                match cachedBackImage with
                | Some b -> b
                | None ->
                    let b =
                        Bitmap(
                            AssetLoader.Open(
                                Uri("avares://GUI_Card_Match/Assets/Cards_Back_Small.png")
                            )
                        )
                    cachedBackImage <- Some b
                    b

            let frontCardImage =
                match cachedFrontImage with
                | Some b -> b
                | None ->
                    let b =
                        Bitmap(
                            AssetLoader.Open(
                                Uri("avares://GUI_Card_Match/Assets/Cards_Front.png")
                            )
                        )
                    cachedFrontImage <- Some b
                    b

            for i in 0 .. state.Board.Length - 1 do

                let card =
                    state.Board[i]

                let button =
                    Button()

                button.Background <- Brushes.Transparent
                button.BorderThickness <- Thickness(0)
                button.Padding <- Thickness(0)

                button.Margin <- Thickness(1.0)

                button.HorizontalAlignment <- HorizontalAlignment.Stretch
                button.VerticalAlignment <- VerticalAlignment.Stretch

                button.Click.Add(fun _ ->
                    this.CardClicked(i)
                )

                let image =
                    Image()

                image.Stretch <- Stretch.Uniform

                if card.Revealed || card.Matched then

                    let layout = Grid()

                    let cardFront =
                        Image(Source = frontCardImage, Stretch = Stretch.Uniform)

                    layout.Children.Add(cardFront) |> ignore

                    let content = StackPanel()
                    content.HorizontalAlignment <- HorizontalAlignment.Center
                    content.VerticalAlignment <- VerticalAlignment.Center
                    content.Spacing <- 4.0

                    let key = $"{card.Theme}/{card.Value}"

                    let bmp =
                        match cachedThemeImages.TryFind(key) with
                        | Some b -> b
                        | None ->
                            let b =
                                Bitmap(
                                    AssetLoader.Open(
                                        Uri($"avares://GUI_Card_Match/Assets/Themes/{card.Theme}/{card.Value}.png")
                                    )
                                )
                            cachedThemeImages <- cachedThemeImages.Add(key, b)
                            b

                    let themeImage =
                        Image(Source = bmp, Stretch = Stretch.Uniform)

                    themeImage.Width <- 2500
                    themeImage.Height <- 2500
                    themeImage.Stretch <- Stretch.Uniform
                    themeImage.HorizontalAlignment <- HorizontalAlignment.Center

                    let text = TextBlock()

                    text.Text <- card.Value
                    text.FontSize <- 500.0
                    text.HorizontalAlignment <- HorizontalAlignment.Center
                    text.VerticalAlignment <- VerticalAlignment.Bottom
                    text.Margin <- Thickness(0.0, 0.0, 0.0, 450.0)
                    text.FontWeight <- FontWeight.SemiBold
                    text.Foreground <- Brushes.Black
                    text.FontFamily <- FontFamily("Comic Sans MS")

                    layout.Children.Add(themeImage) |> ignore
                    layout.Children.Add(text) |> ignore
                    layout.Children.Add(content) |> ignore

                    button.Content <- layout

                else

                    image.Source <- backImage
                    button.Content <- image

                board.Children.Add(button)
                |> ignore

    member this.CardClicked(index : int) =

        if isBusy || aiRunning then
            ()
        else

            match gameState with

            | None ->
                ()

            | Some state ->

                let selectedCard =
                    state.Board[index]

                if selectedCard.Revealed || selectedCard.Matched then
                    ()

                else

                    match firstCard with

                    | None ->

                        let updatedBoard =
                            revealCard state.Board index

                        let card = updatedBoard[index]

                        let updatedSeen =
                            match state.SeenCards.TryFind(card.Value) with
                            | Some s -> s.Add(index)
                            | None -> Set.singleton index

                        gameState <-
                            Some
                                {
                                    state with
                                        Board = updatedBoard
                                        SeenCards =
                                            state.SeenCards.Add(card.Value, updatedSeen)
                                }

                        firstCard <- Some index

                        this.CreateBoard()

                    | Some firstIndex ->

                        let updatedBoard =
                            revealCard state.Board index

                        let card = updatedBoard[index]

                        let updatedSeen =
                            match state.SeenCards.TryFind(card.Value) with
                            | Some s -> s.Add(index)
                            | None -> Set.singleton index

                        let updatedState =
                            {
                                state with
                                    Board = updatedBoard
                                    Attempts = state.Attempts + 1
                                    SeenCards =
                                        state.SeenCards.Add(card.Value, updatedSeen)
                            }

                        gameState <- Some updatedState

                        let attemptCount =
                            this.FindControl<TextBlock>("attemptCount")

                        attemptCount.Text <-
                            updatedState.Attempts.ToString()

                        this.CreateBoard()

                        let first =
                            updatedBoard[firstIndex]

                        let second =
                            updatedBoard[index]

                        if first.Value = second.Value then

                            let matchedBoard =
                                markMatched updatedBoard firstIndex index

                            let finalState =
                                {
                                    updatedState with
                                        Board = matchedBoard
                                }

                            gameState <- Some finalState

                            firstCard <- None

                            this.CreateBoard()

                            if isFinished matchedBoard then

                                let status =
                                    this.FindControl<TextBlock>("GameStatus")

                                status.Text <-
                                    sprintf
                                        "Completed in %d attempts!"
                                        finalState.Attempts

                        else

                            isBusy <- true

                            async {

                                do! Async.Sleep(1000)

                                let hiddenBoard =
                                    hideCards updatedBoard firstIndex index

                                let finalState =
                                    {
                                        updatedState with
                                            Board = hiddenBoard
                                    }

                                gameState <- Some finalState

                                firstCard <- None

                                this.CreateBoard()

                                isBusy <- false
                                aiRunning <- false

                            }
                            |> Async.StartImmediate

    member this.UpdateAttemptText(attempts : int) =

        let attemptCount =
            this.FindControl<TextBlock>("attemptCount")

        attemptCount.Text <- attempts.ToString()

    member this.UpdateStatus(text : string) =

        let status =
            this.FindControl<TextBlock>("GameStatus")

        status.Text <- text

    member this.AIPlay() =

        if isBusy || aiRunning then
            ()
        else

        isBusy <- true
        aiRunning <- true

        let rec aiTurn () =
            async {
                if not aiRunning then
                    isBusy <- false
                    return ()
                else

                match gameState with
                | None ->
                    isBusy <- false
                    aiRunning <- false
                    return ()

                | Some state ->

                    if isFinished state.Board then
                        this.UpdateStatus(sprintf "AI completed in %d attempts!" state.Attempts)
                        isBusy <- false
                        aiRunning <- false
                        return ()
                    else

                    // 플레이어가 한 장 뒤집어놓고 넘긴 경우 → 두 번째 선택부터
                    match firstCard with
                    | Some fIdx ->

                        let fCard = state.Board[fIdx]

                        let knownSecond =
                            match state.SeenCards.TryFind(fCard.Value) with
                            | Some indices ->
                                indices
                                |> Set.toList
                                |> List.tryFind (fun i ->
                                    i <> fIdx && not state.Board[i].Matched)
                            | None -> None

                        let secondOpt =
                            match knownSecond with
                            | Some i -> Some i
                            | None ->
                                state.Board
                                |> List.indexed
                                |> List.tryFind (fun (i, c) ->
                                    i <> fIdx
                                    && not c.Matched
                                    && not c.Revealed)
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

                        // SeenCards에서 이미 아는 짝이 있으면 바로 맞추기
                        let knownPair =
                            state.SeenCards
                            |> Map.tryPick (fun _ indices ->
                                let valid =
                                    indices
                                    |> Set.toList
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
                            let state2 =
                                { state with
                                    Board    = board2
                                    Attempts = state.Attempts + 1 }
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

                            // 모르는 카드 첫 번째 뒤집기
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

                                // 뒤집은 카드의 짝을 이미 알면 바로 맞추기
                                let knownSecond =
                                    match state1.SeenCards.TryFind(fCard.Value) with
                                    | Some indices ->
                                        indices
                                        |> Set.toList
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

                                    // 짝 모름 → 두 번째 모르는 카드 뒤집기
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