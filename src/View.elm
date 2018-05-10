module View exposing (..)

import Model
import Update
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


showCard : Model.Card -> String
showCard c =
    let
        suit =
            case c.suit of
                Model.Club ->
                    "♣"

                Model.Heart ->
                    "♥"

                Model.Spade ->
                    "♠"

                Model.Diamond ->
                    "♦"

        number =
            case c.number of
                1 ->
                    "A"

                11 ->
                    "J"

                12 ->
                    "Q"

                13 ->
                    "K"

                n ->
                    toString n
    in
        suit ++ " " ++ number


showResult : Maybe Model.Result -> String
showResult result =
    case result of
        Just Model.YouWin ->
            "You Win!"

        Just Model.DealerWins ->
            "You Lose!"

        Just Model.YouBust ->
            "You've Busted!"

        Just Model.DealerBust ->
            "The Dealer Has Busted! You Win!"

        Just Model.Draw ->
            "Draw"

        Just Model.Error ->
            "Error!"

        Nothing ->
            "Error!"


view : Model.Model -> Html Model.Msg
view m =
    let
        statusText =
            case m.status of
                Model.Title ->
                    "Welcome To Black Jack"

                Model.UsersTurn ->
                    "Black Jack: Your Turn. Hit or Stand"

                Model.Over ->
                    "Black Jack: Game Over: " ++ (showResult m.result)

        dealersCards =
            case m.status of
                Model.UsersTurn ->
                    List.reverse m.dealer
                        |> List.head
                        |> Maybe.andThen (showCard >> Just)
                        |> Maybe.withDefault ""
                        |> flip (++) " ?"

                Model.Over ->
                    List.reverse m.dealer |> List.map showCard |> String.join " "

                _ ->
                    ""

        playersCards =
            List.reverse m.player |> List.map showCard |> String.join " "

        playersPoint =
            Maybe.map toString m.playersPoint |> Maybe.withDefault "Busted!"

        dealersPoint =
            Maybe.map toString m.dealersPoint |> Maybe.withDefault "Busted!"

        buttons =
            case m.status of
                Model.Title ->
                    [ button [ onClick Model.GameStart ]
                        [ text "Start" ]
                    ]

                Model.UsersTurn ->
                    [ button [ onClick Model.Hit ]
                        [ text "Hit" ]
                    , button [ onClick Model.Stand ]
                        [ text "Stand" ]
                    ]

                Model.Over ->
                    [ button [ onClick Model.GameStart ]
                        [ text "Retry" ]
                    ]
    in
        div []
            [ p []
                [ text statusText ]
            , div [ hidden <| m.status == Model.Title ]
                [ p []
                    [ text ("Dealer's Card: " ++ dealersCards) ]
                , p []
                    [ text ("Player's Cards: " ++ playersCards) ]
                , p []
                    [ text ("Player's Point: " ++ playersPoint) ]
                , p [ hidden <| m.status /= Model.Over ]
                    [ text ("Dealer's Point: " ++ dealersPoint) ]
                ]
            , div []
                buttons
            ]
