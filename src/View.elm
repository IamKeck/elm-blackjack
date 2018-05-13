module View exposing (..)

import Model
import Update
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Debug


cardHtml : String -> Html.Html Model.Msg
cardHtml image_name =
    let
        attr =
            "url(./cards/" ++ image_name ++ ")"
    in
        div
            [ class "card"
            , style
                [ ( "backgroundImage", attr ) ]
            ]
            []


showCard : Model.Card -> Html.Html Model.Msg
showCard c =
    let
        suit =
            case c.suit of
                Model.Club ->
                    "c"

                Model.Heart ->
                    "h"

                Model.Spade ->
                    "s"

                Model.Diamond ->
                    "d"

        number =
            toString c.number
                |> String.cons '0'
                |> String.right 2

        image_name =
            suit ++ number ++ ".png"
    in
        cardHtml image_name


backCard : Html.Html Model.Msg
backCard =
    cardHtml "z01.png"


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
                        |> Maybe.andThen (showCard >> (flip (::) [ backCard ]) >> Just)
                        |> Maybe.withDefault ([ Html.text "" ])

                Model.Over ->
                    List.reverse m.dealer |> List.map showCard

                _ ->
                    Html.text "" |> List.singleton

        playersCards =
            List.reverse m.player |> List.map showCard

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
                    [ text "Dealer's Card: "
                    , div [ class "cardHolder" ]
                        dealersCards
                    ]
                , p []
                    [ text "Player's Cards: "
                    , div [ class "cardHolder" ]
                        playersCards
                    ]
                , p []
                    [ text ("Player's Point: " ++ playersPoint) ]
                , p [ hidden <| m.status /= Model.Over ]
                    [ text ("Dealer's Point: " ++ dealersPoint) ]
                ]
            , div []
                buttons
            ]
