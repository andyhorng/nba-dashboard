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
  , createdAt : String
  , odd : Odd
  }

type alias TotalOdd =
  { score : String
  , oddUnder : String
  , oddOver : String
  , createdAt : String
  }

type alias SpreadOdd =
  { scoreA : String
  , scoreB : String
  , oddA : String
  , oddB : String
  , createdAt : String
  }

type alias MoneyLineOdd =
  { oddA : String
  , oddB : String
  , createdAt : String
  }


type Odd 
  = Total TotalOdd
  | Spread SpreadOdd
  | MoneyLine MoneyLineOdd
  | Unknown


type alias CompetitionToken = String
type alias Model = {
  total: Dict.Dict CompetitionToken Competition,
  spread : Dict.Dict CompetitionToken Competition,
  moneyLine : Dict.Dict CompetitionToken Competition
  }


-- Competition Model 
type alias Source = String
type alias Competition =
  { competitionToken : CompetitionToken
  , teamA : String
  , teamB : String
  , time : String
  , league : String
  , region : String
  , sport : String
  , odds : Dict.Dict Source (List Odd)
  }

addBet : Bet -> Model -> Model
addBet bet model = 
  let 
      updateRow dict = 
        case Dict.get bet.competitionToken dict of 
          Just competition ->
            let oldOdds = Dict.get bet.source competition.odds
            in case oldOdds of 
              Just oldOdds' ->
                {competition | odds = Dict.insert bet.source (oldOdds' ++ [bet.odd]) competition.odds}
              Nothing ->
                {competition | odds = Dict.insert bet.source [bet.odd] competition.odds}

          Nothing ->
            { teamA = bet.teamA
            , teamB = bet.teamB
            , time = bet.time
            , league = bet.league
            , region = bet.region
            , sport = bet.sport
            , odds = Dict.singleton bet.source [bet.odd]
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
  , createdAt = ""
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



historyView : String -> List (String, String) -> Html
historyView current history = 
  div [class "tooltip-item"] 
    [ text current
    , div [class "tooltip"]
        [p [] 
          [ ul [] <| List.map (\(date, data)-> li [] [span [] [text date], span [class "num"] [text data]]) history
          ]
        ]
    ]


totalRow : Competition -> Html
totalRow row = 
  let 
      mapper getter odd = 
        case odd of
          Total data ->
            formatDate data.createdAt `Maybe.andThen` (\date -> Just (date, getter data))
          _ ->
            Nothing

      showOdd : (String, List Odd) -> Maybe Html
      showOdd (source, odds) = 
        let 
            odd = List.head (List.reverse odds)
        in
           case odd of 
              Just odd' ->
                case odd' of 
                  Total data ->
                    Just <| 
                      div [class "stats"] 
                        [ ul [] 
                          [ li [] [text <| translateSource source, span [] [text "盤"]]
                          , li [] [historyView data.score (List.filterMap (mapper .score) odds), span [] [(text "總分")]]
                          , li [] [historyView data.oddOver (List.filterMap (mapper .oddOver) odds), span [] [(text "大於")]]
                          , li [] [historyView data.oddUnder (List.filterMap (mapper .oddOver) odds), span [] [(text "小於")]]
                          ]
                        ]

                  _ ->
                    Nothing
              Nothing ->
                Nothing

      sort = List.sortBy fst
  in
    div [class "row"] 
      [ competitionInfo row
      , div [class "odd"] (List.filterMap showOdd (Dict.toList row.odds) )
      ]

spreadRow : Competition -> Html
spreadRow row = 
  let 
      showOdd : (String, List Odd) -> Maybe Html
      showOdd (source, odds) = 
        let 
            latest = List.head (List.reverse odds)
            getter getter1 getter2 oddRecord = (getter1 oddRecord) ++ "/" ++ (getter2 oddRecord)

            mapper getter odd = 
              case odd of
                Spread data ->
                  formatDate data.createdAt `Maybe.andThen` (\date -> Just (date, getter data))
                _ ->
                  Nothing
        in
            case latest of 
              Just odd' ->
                case odd' of 
                  Spread data ->
                    Just <|
                      div [class "stats"] 
                        [ ul [] 
                          [ li [] [text <| translateSource source, span [] [text "盤"]]
                          , li [] [historyView (getter .scoreA .scoreB data) 
                              (List.filterMap (mapper (getter .scoreA .scoreB)) odds), span [] [(text "讓分")]]

                          , li [] [historyView (getter .oddA .oddB data) 
                              (List.filterMap (mapper (getter .oddA .oddB)) odds), span [] [(text "賠率")]]
                          ]
                        ]
                  _ ->
                    Nothing
              _ ->
                Nothing
  in
    div [class "row"] 
      [ competitionInfo row
      , div [class "odd"] (List.filterMap showOdd (Dict.toList row.odds) )
      ]

moneyLineRow row = 
  let 
      showOdd : (String, List Odd) -> Maybe Html
      showOdd (source, odds) = 
        let 
            latest = List.head (List.reverse odds)
            getter getter1 getter2 oddRecord = (getter1 oddRecord) ++ "/" ++ (getter2 oddRecord)

            mapper getter odd = 
              case odd of
                MoneyLine data ->
                  formatDate data.createdAt `Maybe.andThen` (\date -> Just (date, getter data))
                _ ->
                  Nothing
        in
            case latest of 
              Just odd' ->
                case odd' of 
                  MoneyLine data ->
                    Just <|
                      div [class "stats"] 
                        [ ul [] 
                          [ li [] [text <| translateSource source, span [] [text "盤"]]
                          , li [] [historyView (getter .oddA .oddB data) 
                              (List.filterMap (mapper (getter .oddA .oddB)) odds), span [] [(text "賠率")]]
                          ]
                        ]
                  _ ->
                    Nothing
              _ ->
                Nothing
  in
    div [class "row"] 
      [ competitionInfo row
      , div [class "odd"] (List.filterMap showOdd (Dict.toList row.odds) )
      ]

competitionInfo : Competition -> Html
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
    [ div [class "type"] <| (h3 [] [text "大小分"]) :: List.map totalRow (Dict.values model.total) 
    , div [class "type"] <| (h3 [] [text "讓分"]) :: List.map spreadRow (Dict.values model.spread) 
    , div [class "type"] <| (h3 [] [text "不讓分"]) :: List.map moneyLineRow (Dict.values model.moneyLine) 
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

oddDecoder : String -> String -> Decoder Odd
oddDecoder oddType date = 
    case oddType of 
      "total" ->
        map Total
        <| object4 TotalOdd
          ("score" := string)
          ("odd_under" := string)
          ("odd_over" := string)
          (succeed date)
      "spread" ->
        map Spread
        <| object5 SpreadOdd
          ("score_a" := string)
          ("score_b" := string)
          ("odd_a" := string)
          ("odd_b" := string)
          (succeed date)
      "money_line" ->
        map MoneyLine
        <| object3 MoneyLineOdd
          ("odd_a" := string)
          ("odd_b" := string)
          (succeed date)
      _ -> fail "Unknow odd type"

betDecoder : Decoder Bet
betDecoder =
  ("odd_type" := string) 
  `andThen` (\s -> 
    (at ["meta", "created_at"] string)
      `andThen` \date -> (constructing Bet
        `apply` ("odd_type" := string)
        `apply` ("team_a" := string)
        `apply` ("team_b" := string)
        `apply` ("time" := string)
        `apply` ("league" := string)
        `apply` ("region" := string)
        `apply` ("sport" := string)
        `apply` ("source" := string)
        `apply` ("competition_token" := string)
        `apply` (at ["meta", "created_at"] string)
        `apply` ("odd" := oddDecoder s date)))

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
