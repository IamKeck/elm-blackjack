module Update exposing (..)

import Model
import Random
import Debug


update : Model.Msg -> Model.Model -> ( Model.Model, Cmd Model.Msg )
update msg model =
    case msg of
        Model.GameStart ->
            let
                cmd =
                    Model.sortListGenerator Model.deck |> Random.generate Model.GotShuffleList

                nm =
                    { model | player = [], dealer = [], result = Nothing }
            in
                ( nm, cmd )

        Model.GotShuffleList xs ->
            let
                newDeck =
                    Model.shuffleCards Model.deck xs

                a =
                    toString newDeck
            in
                case newDeck of
                    a :: b :: c :: d :: xs ->
                        let
                            dp =
                                Model.calcValidPoint [ a, c ]

                            pp =
                                Model.calcValidPoint [ b, d ]

                            result =
                                if (dp == Just 21) && (pp == Just 21) then
                                    Just Model.Draw
                                else if pp == Just 21 then
                                    Just Model.YouWin
                                else if dp == Just 21 then
                                    Just Model.DealerWins
                                else
                                    Nothing

                            status =
                                case result of
                                    Just _ ->
                                        Model.Over

                                    Nothing ->
                                        Model.UsersTurn

                            player =
                                d :: b :: model.player

                            dealer =
                                a :: c :: model.dealer
                        in
                            ( { model
                                | deck = xs
                                , status = status
                                , result = result
                                , player = player
                                , dealer = dealer
                                , playersPoint = Model.calcValidPoint player
                                , dealersPoint = Model.calcValidPoint dealer
                              }
                            , Cmd.none
                            )

                    _ ->
                        ( { model | status = Model.Over, result = Just Model.Error }, Cmd.none )

        Model.Hit ->
            case model.deck of
                newCard :: newDeck ->
                    let
                        newPlayerHand =
                            newCard :: model.player

                        newPoint =
                            Model.calcValidPoint newPlayerHand

                        newModel =
                            { model | player = newPlayerHand, playersPoint = newPoint }

                        finalModel =
                            case newPoint of
                                Nothing ->
                                    { newModel | result = Just Model.YouBust, playersPoint = Nothing, status = Model.Over }

                                Just np ->
                                    { newModel | playersPoint = Just np }
                    in
                        ( finalModel, Cmd.none )

                [] ->
                    ( { model | status = Model.Over, result = Just Model.Error }, Cmd.none )

        Model.Stand ->
            let
                newModel =
                    dealersTurn model

                ( result, playersPoint, dealersPoint ) =
                    Model.judge newModel
            in
                ( { newModel
                    | status = Model.Over
                    , result = Just result
                    , playersPoint = playersPoint
                    , dealersPoint = dealersPoint
                  }
                , Cmd.none
                )


dealersTurn : Model.Model -> Model.Model
dealersTurn m =
    case Model.calcValidPoint (m.dealer) of
        Nothing ->
            m

        Just p ->
            if p < 17 then
                case m.deck of
                    nc :: nd ->
                        dealersTurn { m | dealer = nc :: m.dealer, deck = nd }

                    [] ->
                        m
            else
                m
