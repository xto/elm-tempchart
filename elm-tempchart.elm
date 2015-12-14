import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)




view : String -> Result String (List String) -> Html
view string result =
  let queryInput =
        input
          [placeholder "enter whatever"
          , value string
          , on "input" targetValue (Signal.message query.address)
          , myStyle
          ]
          []
      tempSpan =
        case result of
          Err msg ->
            [ div [myStyle] [text msg]]
          Ok temps ->
            List.map (\temp -> span [myStyle][text temp]) temps
  in
    div[] (queryInput :: tempSpan)


myStyle : Attribute
myStyle =
  style
      [ ("width", "100%")
      , ("height", "40px")
      , ("padding", "10px 0")
      , ("font-size", "2em")
      , ("text-align", "center")
      ]

--Wiring
main : Signal Html
main =
  Signal.map2 view query.signal results.signal

query : Signal.Mailbox String
query =
  Signal.mailbox ""

results: Signal.Mailbox (Result String (List String))
results =
  Signal.mailbox (Err "Potato")

port updateContent : Signal (Task x ())
port updateContent =
  Signal.map getTemp query.signal
    |> Signal.map (\task -> Task.toResult task `andThen` Signal.send results.address)

getTemp : String -> Task String (List String)
getTemp query =
  let testUrl =
      if String.length query /= 0
        then succeed ("http://localhost:3000/" ++ query)
        else fail "Matt Damon needs 1+ char"
  in
    testUrl `andThen` (mapError (always "BOOM") << Http.get jd)

jd : Json.Decoder (List String)
jd =
  let tempobj =
    Json.object1 (\temperature-> temperature ++ "C")
        ("temperature" := Json.string)
  in
    "temperatures" := Json.list tempobj
