module Main where

import Html exposing (..)
import Json.Decode exposing (..)
import Json.Encode
import Signal exposing (Signal, Address)
import SocketIO
import Dict
import Task exposing (Task)

-- model

type alias Bet = 
  { oddType : String
  , teamA : String
  , teamB : String
  , time : String
  , league : String
  , region : String
  , sport : String
  , source : String
  , competitionToken : String
  , odd : Odd
  }

type alias TotalOdd =
  { score : String
  , oddUnder : String
  , oddOver : String
  }

type alias SpreadOdd =
  { scoreA : String
  , scoreB : String
  , oddA : String
  , oddB : String
  }

type alias MoneyLineOdd =
  { oddA : String
  , oddB : String
  }


type Odd 
  = Total TotalOdd
  | Spread SpreadOdd
  | MoneyLine MoneyLineOdd
  | Unknown


type alias CompetitionToken = String
type alias Model = {
  total: Dict.Dict CompetitionToken Row,
  spread : Dict.Dict CompetitionToken Row,
  moneyLine : Dict.Dict CompetitionToken Row
  }


-- Row Model 
type alias Source = String
type alias Row =
  { competitionToken : CompetitionToken
  , teamA : String
  , teamB : String
  , time : String
  , league : String
  , region : String
  , sport : String
  , odds : Dict.Dict Source Odd
  }

addBet : Bet -> Model -> Model
addBet bet model = 
  let 
      updateRow dict = 
        case Dict.get bet.competitionToken dict of 
          Just row ->
            {row | odds = Dict.insert bet.source bet.odd row.odds}
          Nothing ->
            { teamA = bet.teamA
            , teamB = bet.teamB
            , time = bet.time
            , league = bet.league
            , region = bet.region
            , sport = bet.sport
            , odds = Dict.singleton bet.source bet.odd
            , competitionToken = bet.competitionToken}

  in
    case bet.oddType of 
      "total" -> 
        {model | total = Dict.insert bet.competitionToken (updateRow model.total) model.total}
      "spread" -> 
        {model | spread = Dict.insert bet.competitionToken (updateRow model.spread) model.spread}
      "money_line" -> 
        {model | moneyLine = Dict.insert bet.competitionToken (updateRow model.moneyLine)  model.moneyLine}
      _ ->
        model
  

initialModel : Model
initialModel = {
  total = Dict.empty,
  spread = Dict.empty,
  moneyLine = Dict.empty}

emptyBet = { 
  teamA = ""
  , teamB = ""
  , oddType = ""
  , time = ""
  , league = ""
  , region = ""
  , sport = ""
  , source = ""
  , competitionToken = ""
  , odd = Unknown}

-- view

totalRow : Row -> Html
totalRow row = 
  let 
      showOdd : Odd -> Maybe Html
      showOdd odd = 
        case odd of 
          Total data ->
            Just <| tr [] 
              [ td [] [(text <| toString <| data.score)]
              , td [] [(text data.oddUnder)]
              , td [] [(text data.oddOver)] ]
          _ ->
            Nothing
  in
    tr [] 
      [ td [] [ text row.teamA ] 
      , td [] [ text row.teamB ]
      , td [] [ text row.time ]
      , td [] (List.filterMap showOdd (Dict.values row.odds) )
      ]

spreadRow : Row -> Html
spreadRow row = 
  let 
      showOdd : Odd -> Maybe Html
      showOdd odd = 
        case odd of 
          Spread data ->
            Just <| tr [] 
              [ td [] [(text <| toString <| data.scoreA)]
              , td [] [(text <| toString <| data.scoreB)]
              , td [] [(text data.oddA)]
              , td [] [(text data.oddB)] ]
          _ ->
            Nothing
  in
    tr [] 
      [ td [] [ text row.teamA ] 
      , td [] [ text row.teamB ]
      , td [] [ text row.time ]
      , td [] (List.filterMap showOdd (Dict.values row.odds))
      ]

moneyLineRow : Row -> Html
moneyLineRow row = 
  let 
      showOdd : Odd -> Maybe Html
      showOdd odd = 
        case odd of 
          MoneyLine data ->
            Just <| tr [] 
              [ td [] [(text data.oddA)]
              , td [] [(text data.oddB)]
              ]
          _ ->
            Nothing
  in
    tr [] 
      [ td [] [ text row.teamA ] 
      , td [] [ text row.teamB ]
      , td [] [ text row.time ]
      , td [] (List.filterMap showOdd (Dict.values row.odds) )
      ]

view : Address Action -> Model -> Html
view address model = 
  div [] 
    [ table [] 
      [ (thead [] [tr [] 
        [ th [] [text "隊伍A"]
        , th [] [text "隊伍B"]
        , th [] [text "時間"]
        , th [] [text "賠率"]]])
      , (tbody [] (List.map totalRow (Dict.values model.total)))]

    , table [] 
      [ (thead [] [tr [] 
        [ th [] [text "隊伍A"]
        , th [] [text "隊伍B"]
        , th [] [text "時間"]
        , th [] [text "賠率"]]])
      , (tbody [] (List.map spreadRow (Dict.values model.spread)))]

    , table [] 
      [ (thead [] [tr [] 
        [ th [] [text "隊伍A"]
        , th [] [text "隊伍B"]
        , th [] [text "時間"]
        , th [] [text "賠率"]]])
      , (tbody [] (List.map moneyLineRow (Dict.values model.moneyLine)))]
    ]


-- update
type Action 
  = NoOp
  | Update Bet

update : Action -> Model -> Model
update action model = 
  case action of 

    NoOp -> model

    Update bet ->
      addBet bet model

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

-- Applicative's `pure`:
constructing : a -> Decoder a
constructing = succeed

-- Applicative's `<*>`:
apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply = object2 (<|)

socket : Task x SocketIO.Socket
socket = SocketIO.io "" SocketIO.defaultOptions
rawBets : Signal.Mailbox String
rawBets = Signal.mailbox "null"

port responses : Task x ()
port responses = socket `Task.andThen` SocketIO.on "update" rawBets.address

oddDecoder : String -> Decoder Odd
oddDecoder oddType = 
  case oddType of 
    "total" ->
      map Total
      <| object3 TotalOdd
        ("score" := string)
        ("odd_under" := string)
        ("odd_over" := string)
    "spread" ->
      map Spread
      <| object4 SpreadOdd
        ("score_a" := string)
        ("score_b" := string)
        ("odd_a" := string)
        ("odd_b" := string)
    "money_line" ->
      map MoneyLine
      <| object2 MoneyLineOdd
        ("odd_a" := string)
        ("odd_b" := string)
    _ -> fail "Unknow odd type"

betDecoder : Decoder Bet
betDecoder =
  ("odd_type" := string) 
  `andThen` \s -> (constructing Bet
    `apply` ("odd_type" := string)
    `apply` ("team_a" := string)
    `apply` ("team_b" := string)
    `apply` ("time" := string)
    `apply` ("league" := string)
    `apply` ("region" := string)
    `apply` ("sport" := string)
    `apply` ("source" := string)
    `apply` ("competition_token" := string)
    `apply` ("odd" := oddDecoder s))

log : Result String a -> Result String a
log r = 
  case r of 
    Ok _ -> r
    Err x -> 
      Debug.log x
      r

bets : Signal Action
bets 
  = (Signal.filterMap ((decodeString betDecoder)>>log>>Result.toMaybe) emptyBet rawBets.signal)
  |> Signal.map Update

-- Here is the magic happens
model : Signal Model
model =
  Signal.foldp update initialModel bets

main : Signal Html
main = 
  Signal.map (view actions.address) model
