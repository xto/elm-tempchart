import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp.Simple as StartApp

-- Model

type alias TemperatureReading =
  {temperature: String, readAt: String, unit: String}

type alias Model = {
  temperatureReadings: List TemperatureReading
}

initialModel : Model
initialModel =
  { temperatureReadings = [
    {temperature = "102", readAt = "NOW", unit = "Kelvin"}
  ] }

--- Update
type Action
  = NoOp
  | LoadReadings

update: Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    LoadReadings ->
      -- let
      --   newReadings =

      {model | temperatureReadings <- List.append model.temperatureReadings [{temperature = "102", readAt = "NOW", unit = "Kelvin"}]}


-- View
-- view : String -> Result String (List String) -> Html
-- view string result =
--   let queryInput =
--         input
--           [placeholder "enter whatever"
--           , value string
--           , on "input" targetValue (Signal.message query.address)
--           , myStyle
--           ]
--           []
--       tempSpan =
--         case result of
--           Err msg ->
--             [ div [myStyle] [text msg]]
--           Ok temps ->
--             List.map (\temp -> span [myStyle][text temp]) temps
--   in
--     div[] (queryInput :: tempSpan)
--
view: Signal.Address Action -> Model ->Html
view address model =
  div [][
  button [onClick address LoadReadings][text "POTATO"]
  ,text (toString model.temperatureReadings)]

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
main: Signal Html
main =
  StartApp.start {model = initialModel, view = view, update = update}


query : Signal.Mailbox String
query =
  Signal.mailbox ""

results: Signal.Mailbox (Result String (List String))
results =
  Signal.mailbox (Err "Potato")

-- port updateContent : Signal (Task String (List TemperatureReading))
-- port updateContent =
  -- Signal.map getTemp query.signal
  --Signal.map (\task -> Task.toResult task `andThen` Signal.send results.address)

getTemp : Task String (List TemperatureReading)
getTemp =
  let urlTask =
        succeed ("http://localhost:3000/")
  in
    urlTask `andThen` httpGetWrapper

httpGetWrapper : String -> Task String (List TemperatureReading)
httpGetWrapper urlString =
      (mapError (always "BOOM") (getUrl urlString))

getUrl : String -> Task Http.Error (List TemperatureReading)
getUrl urlString =
  Http.get jd urlString


jd : Json.Decoder (List TemperatureReading)
jd =
  let tempobj =
    Json.object3
        TemperatureReading
        ("temperature" := Json.string)
        ("readAt" := Json.string)
        ("unit" := Json.string)
  in
    "temperatures" := Json.list tempobj
