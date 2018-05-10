module Model exposing (..)

import Tuple
import Random
import Debug


type alias Model =
    { deck : List Card
    , player : List Card
    , dealer : List Card
    , status : GameStatus
    , result : Maybe Result
    , playersPoint : Maybe Int
    , dealersPoint : Maybe Int
    }


type Msg
    = GameStart
    | Hit
    | Stand
    | GotShuffleList (List Int)


type GameStatus
    = Title
    | UsersTurn
    | Over


type Result
    = YouWin
    | DealerWins
    | YouBust
    | DealerBust
    | Draw
    | Error


initialModel =
    { deck = [], player = [], dealer = [], status = Title, result = Nothing, playersPoint = Nothing, dealersPoint = Nothing }


judge : Model -> ( Result, Maybe Int, Maybe Int )
judge m =
    case ( Debug.log "yp " (calcValidPoint m.player), Debug.log "dp" (calcValidPoint m.dealer) ) of
        ( Nothing, dp ) ->
            ( DealerWins, Nothing, dp )

        ( yp, Nothing ) ->
            ( YouWin, yp, Nothing )

        ( Just yp, Just dp ) ->
            if yp == dp then
                ( Draw, Just yp, Just dp )
            else if yp > dp then
                ( YouWin, Just yp, Just dp )
            else
                ( DealerWins, Just yp, Just dp )



-- Card


type Suit
    = Spade
    | Heart
    | Club
    | Diamond


type alias Card =
    { suit : Suit, number : Int }


allNums : List Int
allNums =
    List.range 1 13


allSuits : List Suit
allSuits =
    [ Spade, Heart, Club, Diamond ]


deck : List Card
deck =
    List.concatMap (\s -> List.map (Card s) allNums) allSuits


cardToPointAceAsOne : Card -> Int
cardToPointAceAsOne c =
    if c.number > 10 then
        10
    else
        c.number


cardToPointAceAsEleven : Card -> Int
cardToPointAceAsEleven c =
    if c.number == 1 then
        11
    else
        cardToPointAceAsOne c


calcPoint : (Card -> Int) -> List Card -> Int
calcPoint f cs =
    List.map f cs |> List.sum


calcValidPoint : List Card -> Maybe Int
calcValidPoint cs =
    let
        high =
            calcPoint cardToPointAceAsEleven cs

        low =
            calcPoint cardToPointAceAsOne cs
    in
        if low > 21 then
            Nothing
        else if high > 21 then
            Just low
        else
            Just high


shuffleCards : List Card -> List Int -> List Card
shuffleCards cs xs =
    List.map2 (,) cs xs |> List.sortBy Tuple.second |> List.map Tuple.first


sortListGenerator : List Card -> Random.Generator (List Int)
sortListGenerator cs =
    let
        cs_l =
            List.length cs
    in
        cs_l * 100 |> Random.int 0 |> Random.list cs_l
