import Http

import Json.Decode as Json exposing ((:=))

import Task exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import String

-- View

view : String -> Result String (List String) -> Html
view string result =
  let message = 
    case result of 
      Err msg ->
        [div [] [ text "ERROR" ]]
      Ok temp ->
        [span [] [text temp]]

  in
    div [] (message)

-- WIRING

main =
  Signal.map2 view query.signal result.signal

query : Signal.Mailbox String
query = 
  Signal.mailbox ""

result : Signal.Mailbox (Result String (List String))
result = 
  Signal.mailbox (Err "I'm a tomato!")

port request : Signal (Task x ())
port request = 
  Signal.map getTemp query.signal
    |> Signal.map (\task -> Task.toResult task `andThen` Signal.send result.address)

getTemp : Task String (List String)
getTemp  = 
  "http://localhost:3000/getTemp" `andThen` (mapError (always "BOOM !") << Http.get temp)

temp : Json.Decoder (List String)
temp = 
  let tempAtTime = 
    Json.object2 (\temperature time -> temperature ++ ", " ++ time)
      ("temperature reading" := Json.string)
      ("time" := Json.string)
  in 
    "temperatureData" := Json.list tempAtTime
