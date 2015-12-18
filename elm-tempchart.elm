import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp
import Effects exposing (Effects, Never)

-- Model

type alias TemperatureReading =
  {temperature: String, readAt: String, unit: String}

type alias Model = {
  temperatureReadings: List TemperatureReading
}

--- Update
type Action
  = NoOp
  | LoadReadings

update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
      (model
      , Effects.none)

    LoadReadings ->
      ( Model model.temperatureReadings
      ,  Effects.none
      )

view: Signal.Address Action -> Model ->Html
view address model =
  div [][
  button [onClick address LoadReadings][text "POTATO"]
  ,div [] [(text (toString model.temperatureReadings))]]

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
app :{html : Signal Html, model : Signal Model, tasks : Signal (Task Effects.Never())}
app =
  StartApp.start
    {init = init
    ,update = update
    ,view = view
    ,inputs = []
    }
init : (Model, Effects Action)
init =
  (Model [{temperature = "102", readAt = "NOW", unit = "Kelvin"}]
  , Effects.none)

main: Signal Html
main =
  app.html

getTemp : Effects Action
getTemp =
  Http.get jd "http://localhost:3000/"
    |> Task.toMaybe
    |> Task.map LoadReadings
    |> Effects.task

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


-- query : Signal.Mailbox String
-- query =
--   Signal.mailbox ""
--
-- results: Signal.Mailbox (Result String (List String))
-- results =
--   Signal.mailbox (Err "Potato")

-- port updateContent : Signal (Task String (List TemperatureReading))
-- port updateContent =
  -- Signal.map getTemp query.signal
  --Signal.map (\task -> Task.toResult task `andThen` Signal.send results.address)

-- getTemp : Task String (List TemperatureReading)
-- getTemp =
--   let urlTask =
--         succeed ("http://localhost:3000/")
--   in
--     urlTask `andThen` httpGetWrapper

-- getTemp : Effects (Maybe (List TemperatureReading))


--
-- httpGetWrapper : String -> Task String (List TemperatureReading)
-- httpGetWrapper urlString =
--       (mapError (always "BOOM") getUrl urlString)
