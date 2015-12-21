module ElmTempchart where
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


init : String -> (Model, Effects Action)
init some_String=
  (Model [{temperature = some_String, readAt = some_String, unit = some_String}]
  , getTemp)


--- Update

type Action
  = RequestReadings
  | LoadReadings (Maybe (List TemperatureReading))

update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    RequestReadings ->
      (model
      , getTemp)

    LoadReadings newTemperatureReadings->
      let
        eval_readings =
          (Maybe.withDefault [] newTemperatureReadings)
      in
        ( Model (List.append model.temperatureReadings eval_readings)
          ,Effects.none
        )


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
getTemp : Effects Action
getTemp =
  Http.get jd (Http.url "http://localhost:3000/" [("Potato", "Chip")])
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

-- Main
app :{html : Signal Html, model : Signal Model, tasks : Signal (Task Effects.Never())}
app =
  StartApp.start
    { init = init "funny cats"
    , update = update
    , view = view
    , inputs = []
    }


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

main: Signal Html
main =
  app.html
