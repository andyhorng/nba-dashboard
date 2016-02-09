module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (..)
import Json.Encode
import Signal exposing (Signal, Address)
import SocketIO
import Dict
import Task exposing (Task)
import Date.Format
import Date
import String
import List
import Array

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

formatDate : String -> Maybe String
formatDate orig = 
  case Date.fromString orig of 
    Ok date ->
      Just <| Date.Format.format "%Y/%m/%d %H:%M" date
    Err s ->
      Debug.log s
      Nothing

translateSource : String -> String
translateSource source = 
  case source of 

    "twsport" ->
      "台彩"

    "bet365" ->
      "國際"

    _ ->
      "Unknown"

translateTeamName : String -> String
translateTeamName raw = 
  let
      last : Array.Array String -> Maybe String
      last array = Array.get ((Array.length array)-1) array
      key = String.split " " (String.toLower raw) |> Array.fromList |> last
  in
      case key of
        Nothing ->
          "Unknown"

        Just s ->
          case s of
            "76ers" -> 
              "76人"
            "blazers" ->
              "拓荒者"
            "bucks" ->
              "公鹿"
            "bulls" ->
              "公牛"
            "cavaliers" ->
              "騎士"
            "celtics" ->
              "塞爾提克"
            "clippers" ->
              "快艇"
            "grizzlies" ->
              "灰熊"
            "hawks" ->
              "老鷹"
            "heat" ->
              "熱火"
            "hornets" ->
              "黃蜂"
            "jazz" ->
              "爵士"
            "kings" ->
              "國王"
            "knicks" ->
              "尼克"
            "lakers" ->
              "湖人"
            "magic" ->
              "魔術"
            "mavericks" ->
              "小牛"
            "nets" ->
              "籃網"
            "nuggets" ->
              "金塊"
            "pacers" ->
              "溜馬"
            "pelicans" ->
              "鵜鶘"
            "pistons" ->
              "活塞"
            "raptors" ->
              "暴龍"
            "rockets" ->
              "火箭"
            "spurs" ->
              "馬刺"
            "suns" ->
              "太陽"
            "thunder" ->
              "雷霆"
            "timberwolves" ->
              "灰狼"
            "warriors" ->
              "勇士"
            "wizards" ->
              "巫師"
            _ -> 
              "不知道"


totalRow : Row -> Html
totalRow row = 
  let 
      showOdd : (String, Odd) -> Maybe Html
      showOdd (source, odd) = 
        case odd of 
          Total data ->
            Just <| 
              div [class "stats"] 
                [ ul [] 
                  [ li [] [text <| translateSource source, span [] [text "盤"]]
                  , li [] [text data.score, span [] [(text "總分")]]
                  , li [] [text data.oddOver, span [] [(text "大於")]]
                  , li [] [text data.oddUnder, span [] [(text "大於")]]
                  ]
                ]

          _ ->
            Nothing
  in
    div [class "row"] 
      [ competitionInfo row
      , div [class "odd"] (List.filterMap showOdd (Dict.toList row.odds) )
      ]

spreadRow : Row -> Html
spreadRow row = 
  let 
      showOdd : (String, Odd) -> Maybe Html
      showOdd (source, odd) = 
        case odd of 
          Spread data ->
            Just <|
              div [class "stats"] 
                [ ul [] 
                  [ li [] [text <| translateSource source, span [] [text "盤"]]
                  , li [] [text (data.scoreA ++ "/" ++ data.scoreB), span [] [(text "讓分")]]
                  , li [] [text (data.oddA ++ "/" ++ data.oddB), span [] [(text "賠率")]]
                  ]
                ]
          _ ->
            Nothing
  in
    div [class "row"] 
      [ competitionInfo row
      , div [class "odd"] (List.filterMap showOdd (Dict.toList row.odds) )
      ]

moneyLineRow : Row -> Html
moneyLineRow row = 
  let 
      showOdd : (String, Odd) -> Maybe Html
      showOdd (source, odd) = 
        case odd of 
          MoneyLine data ->
            Just <| 
              div [class "stats"] 
                [ ul [] 
                  [ li [] [text <| translateSource source, span [] [text "盤"]]
                  , li [] [text (data.oddA ++ "/" ++ data.oddB), span [] [(text "賠率")]]
                  ]
                ]
          _ ->
            Nothing
  in
    div [class "row"] 
      [ competitionInfo row
      , div [class "odd"] (List.filterMap showOdd (Dict.toList row.odds) )
      ]

competitionInfo : Row -> Html
competitionInfo row = 
  div [class "competition-info"]
    [ span [class "team-name"] [text <| translateTeamName row.teamA]
    , span [class "vs"] [text "v.s."]
    , span [class "team-name"] [text <| translateTeamName row.teamB] 
    , div [class "time"] [text <| case formatDate row.time of 
        Just dstr -> 
          dstr

        Nothing -> 
          ""
      ] 
    ]

view : Address Action -> Model -> Html
view address model = 
  div [class "container"] 
    [ div [class "type"] <| (h3 [] [text "大小分"]) ::  List.map totalRow (Dict.values model.total) 
    , div [class "type"] <| (h3 [] [text "讓分"]) ::  List.map spreadRow (Dict.values model.spread) 
    , div [class "type"] <| (h3 [] [text "不讓分"]) ::  List.map moneyLineRow (Dict.values model.moneyLine) 
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
