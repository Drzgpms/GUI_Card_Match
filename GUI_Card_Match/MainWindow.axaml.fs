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

type MainWindow() as this =
    inherit Window()

    let mutable isBusy = false

    let mutable gameState : GameState option = None

    let mutable firstCard : int option = None

    do
        this.InitializeComponent()

        let easy =
            this.FindControl<Button>("EasyButton")

        let normal =
            this.FindControl<Button>("NormalButton")

        let hard =
            this.FindControl<Button>("HardButton")

        let newGame =
            this.FindControl<Button>("NewGameButton")

        easy.Click.Add(fun _ -> this.StartGame(4))
        normal.Click.Add(fun _ -> this.StartGame(6))
        hard.Click.Add(fun _ -> this.StartGame(8))

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
                Bitmap(
                    AssetLoader.Open(
                        Uri("avares://GUI_Card_Match/Assets/Cards_Back_Small.png")
                    )
                )
            
            let frontCardImage =
                Bitmap(
                    AssetLoader.Open(
                        Uri(
                            $"avares://GUI_Card_Match/Assets/Cards_Front.png"
                        )
                    )
                )

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

                    let cardFront = Image(Source = frontCardImage, Stretch = Stretch.Uniform)

                    layout.Children.Add(cardFront) |> ignore

                    let content = StackPanel()
                    content.HorizontalAlignment <- HorizontalAlignment.Center
                    content.VerticalAlignment <- VerticalAlignment.Center
                    content.Spacing <- 4.0

                    let themeImage = Image(Source = Bitmap(AssetLoader.Open(Uri($"avares://GUI_Card_Match/Assets/Themes/{card.Theme}/{card.Value}.png"))), Stretch = Stretch.Uniform)

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

        if isBusy then
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

                        gameState <-
                            Some
                                {
                                    state with
                                        Board = updatedBoard
                                }

                        firstCard <- Some index

                        this.CreateBoard()

                    | Some firstIndex ->

                        let updatedBoard =
                            revealCard state.Board index

                        let updatedState =
                            {
                                state with
                                    Board = updatedBoard
                                    Attempts = state.Attempts + 1
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

                            }
                            |> Async.StartImmediate