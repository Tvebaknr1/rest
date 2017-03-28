import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as Decode

type Msg
  = SetCounter
  | CallCounter
  | GetCounter (Result Http.Error String)
  | UpdateCounter (Result Http.Error String)

type alias Model
  = { url : String
    ,counter: Int }

view : Model -> Html Msg
view model =
    div []
      [ button [ onClick GetCounter ] [ text "Get"]
      , button [ onClick UpdateCounter ] [ text "Set"]
      , h2 [] [text (toString model.counter)]
      ]


getCounter : Cmd Msg
getCounter =
  let
    url = "Localhost:8080/Counter"
  in
    Http.send GetCounter (Http.get url decodeGifUrl)

updateCounter : Cmd Msg
updateCounter =
  let
    url = "Localhost:8080/Counter/1"
  in
    Http.send UpdateCounter (Http.get url decodeGifUrl)

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    SetCounter -> (model,updateCounter)
    CallCounter -> (model,getCounter)
    GetCounter (Ok value) -> (Model "" (Result.withDefault 0 (String.toInt value)) , Cmd.none)
    GetCounter (Err _) -> (model, Cmd.none)
    UpdateCounter (Ok value) ->(Model "" 1, Cmd.none)
    UpdateCounter (Err _) -> (model, Cmd.none)

main =
  Html.program
    { init = (Model "" 0, Cmd.none)
    , view = view
    , update = update
    , subscriptions = \x -> Sub.none }

decodeGifUrl : Decode.Decoder String
decodeGifUrl =
  Decode.at ["data", "image_url"] Decode.string
