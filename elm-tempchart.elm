module ElmTempchart where
import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp
import Effects exposing (Effects, Never)
import Time exposing (Time, second)


-- Model
type alias TemperatureReading =
  {temperature: String, readAt: String, unit: String}

type alias Model = {
  temperatureReadings: List TemperatureReading
}


init : (Model, Effects Action)
init =
  (Model []
  , getTemp httpGetCall)


--- Update

type Action
  = RequestReadings
  | LoadReadings (Maybe (List TemperatureReading))
  | RequestReadingsOnTime (Effects Action)

update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    RequestReadings ->
      (model
      , getTemp httpGetCall)

    LoadReadings newTemperatureReadings->
      let
        eval_readings =
          (Maybe.withDefault [] newTemperatureReadings)
      in
        case newTemperatureReadings of
          Nothing ->
            (model, Effects.none)
          Maybe.Just potato ->
            ( Model (List.append model.temperatureReadings eval_readings)
              ,Effects.none
            )
    RequestReadingsOnTime effect ->
      (model, effect)


view: Signal.Address Action -> Model ->Html
view address model =
  div [][
  button [onClick address RequestReadings][text "POTATO"]
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
getTemp : Task a (List TemperatureReading)-> Effects Action
getTemp httpGetCallFunc =
  httpGetCallFunc
    |> Task.toMaybe
    |> Task.map LoadReadings
    |> Effects.task

httpGetCall :Task Http.Error (List TemperatureReading)
httpGetCall  =
  Http.get jd (Http.url "http://localhost:3000/" [])


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
clock: Signal Time
clock =
  Time.every Time.second

httpGet: a -> Effects Action
httpGet t =
  Http.get jd (Http.url "http://localhost:3000/" [])
    |> Task.toMaybe
    |> Task.map LoadReadings
    |> Effects.task


periodicGet: Signal Action
periodicGet =
  Signal.map (httpGet) clock
  |> Signal.map RequestReadingsOnTime


-- Main
app :{html : Signal Html, model : Signal Model, tasks : Signal (Task Effects.Never())}
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [periodicGet]
    }


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

main: Signal Html
main =
  app.html
