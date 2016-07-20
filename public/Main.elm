module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Dict
import Task exposing (Task)
import Date
import Date.Format
import String
import List
import Array
import WebSocket
import Html.App as Html

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
      let
          -- hack...
          twdate = Date.fromTime <| (Date.toTime date) + 8*60*60
      in
          Just <| Date.Format.format "%Y/%m/%d %H:%M" twdate
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



historyView : String -> List (String, String) -> Html Msg
historyView current history =
  div [class "tooltip-item"]
    [ text current
    , div [class "tooltip"]
        [p []
          [ ul [] <| List.map (\(date, data)-> li [] [span [] [text date], span [class "num"] [text data]]) history
          ]
        ]
    ]


totalRow : Competition -> Html Msg
totalRow row =
  let
      mapper getter odd =
        case odd of
          Total data ->
            formatDate data.createdAt `Maybe.andThen` (\date -> Just (date, getter data))
          _ ->
            Nothing

      showOdd : (String, List Odd) -> Maybe (Html Msg)
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

spreadRow : Competition -> Html Msg
spreadRow row =
  let
      showOdd : (String, List Odd) -> Maybe (Html Msg)
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
      showOdd : (String, List Odd) -> Maybe (Html Msg)
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

competitionInfo : Competition -> Html Msg
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

view : Model -> Html Msg
view model =
  div [class "container"]
    [ div [class "type"] <| (h3 [] [text "大小分"]) :: List.map totalRow (Dict.values model.total)
    , div [class "type"] <| (h3 [] [text "讓分"]) :: List.map spreadRow (Dict.values model.spread)
    , div [class "type"] <| (h3 [] [text "不讓分"]) :: List.map moneyLineRow (Dict.values model.moneyLine)
    ]


-- update
type Msg
  = NoOp
  | Update Bet

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    NoOp -> (model, Cmd.none)

    Update bet ->
      (addBet bet model, Cmd.none)

oddDecoder : String -> Decoder Odd
oddDecoder oddType =
    case oddType of
      "total" ->
        map Total
        <| object4 TotalOdd
          ("score" := string)
          ("odd_under" := string)
          ("odd_over" := string)
          ("created_at" := string)
      "spread" ->
        map Spread
        <| object5 SpreadOdd
          ("score_a" := string)
          ("score_b" := string)
          ("odd_a" := string)
          ("odd_b" := string)
          ("created_at" := string)
      "money_line" ->
        map MoneyLine
        <| object3 MoneyLineOdd
          ("odd_a" := string)
          ("odd_b" := string)
          ("created_at" := string)
      _ -> fail "Unknow odd type"

betDecoder : Decoder Bet
betDecoder =
  ("odd_type" := string)
  `andThen` (\s ->
      succeed Bet
        |: ("odd_type" := string)
        |: ("team_a" := string)
        |: ("team_b" := string)
        |: ("time" := string)
        |: ("league" := string)
        |: ("region" := string)
        |: ("sport" := string)
        |: ("source" := string)
        |: ("competition_token" := string)
        |: (at ["meta", "created_at"] string)
        |: ("odd" := oddDecoder s))

log : Result String a -> Result String a
log r =
  case r of
    Ok _ -> r
    Err x ->
      Debug.log x
      r

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen "ws://localhost:8000" decode

decode : String -> Msg
decode str =
  case (decodeString betDecoder >> log >> Result.toMaybe) str of
    Just bet ->
      Update bet
    Nothing ->
      NoOp

init : (Model, Cmd Msg)
init =
  (initialModel, Cmd.none)

main =
  Html.program
    { init = init, update = update, view = view, subscriptions = subscriptions }
