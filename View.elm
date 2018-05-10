module View exposing (..)

import Model
import Update
import Html exposing (..)
import Html.Events exposing (..)


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
    in
        suit ++ " " ++ (toString c.number)


view : Model.Model -> Html Model.Msg
view m =
    case m.status of
        Model.Title ->
            div []
                [ p []
                    [ text "Welcome To Black Jack" ]
                , div []
                    [ button [ onClick Model.GameStart ]
                        [ text "Start" ]
                    ]
                ]

        Model.UsersTurn ->
            let
                dealersCard =
                    List.reverse m.dealer
                        |> List.head
                        |> Maybe.andThen (showCard >> Just)
                        |> Maybe.withDefault ""

                playersCards =
                    List.reverse m.player |> List.map showCard |> String.join " "

                playersPoint =
                    Maybe.map toString m.playersPoint |> Maybe.withDefault "Busted!"
            in
                div []
                    [ p []
                        [ text "Black Jack: Your Turn. Hit or Stand" ]
                    , div []
                        [ p []
                            [ text ("Dealer's Card: " ++ dealersCard ++ " ?") ]
                        , p []
                            [ text ("Player's Cards: " ++ playersCards) ]
                        , p []
                            [ text ("Player's Point: " ++ playersPoint) ]
                        ]
                    , div []
                        [ button [ onClick Model.Hit ]
                            [ text "Hit" ]
                        , button [ onClick Model.Stand ]
                            [ text "Stand" ]
                        ]
                    ]

        Model.Over ->
            let
                dealersCards =
                    List.reverse m.dealer |> List.map showCard |> String.join " "

                playersCards =
                    List.reverse m.player |> List.map showCard |> String.join " "

                dealersPoint =
                    Maybe.map toString m.dealersPoint |> Maybe.withDefault "Busted!"

                playersPoint =
                    Maybe.map toString m.playersPoint |> Maybe.withDefault "Busted!"

                result =
                    case m.result of
                        Just Model.YouWin ->
                            "You Win!"

                        Just Model.DealerWins ->
                            "You Lose!"

                        Just Model.YouBust ->
                            "You've Busted!"

                        Just Model.DealerBust ->
                            "Dealer Has Busted! You Win!"

                        Just Model.Draw ->
                            "Draw"

                        Just Model.Error ->
                            "Error!"

                        Nothing ->
                            "Error!"
            in
                div []
                    [ p []
                        [ text ("Black Jack: Game Over " ++ result) ]
                    , div []
                        [ p []
                            [ text ("Dealer's Cards: " ++ dealersCards) ]
                        , p []
                            [ text ("Player's Cards: " ++ playersCards) ]
                        , p []
                            [ text ("Dealer's Point: " ++ dealersPoint) ]
                        , p []
                            [ text ("Player's Point: " ++ playersPoint) ]
                        ]
                    , div []
                        [ button [ onClick Model.GameStart ]
                            [ text "Retry" ]
                        ]
                    ]
