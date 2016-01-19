import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue, onClick)
import StartApp
import Effects exposing (Effects, Never)
import Time exposing (Time, second)
import Chartjs exposing (..)
import Chartjs.Line exposing (..)
import Color exposing (..)

-- Model
type alias TemperatureReading =
  {temperature: Float, readAt: String, unit: String}


type alias Model = {
  temperatureReadings: List TemperatureReading
  , mashName: String
  , mashNamed: Bool
}


init : (Model, Effects Action)
init =
  ((Model [] "" False)
  , getTempAction)


--- Update

type Action
  = LoadReadings (Maybe (List TemperatureReading))
  | RequestReadings
  | RequestReadingsOnTime (Effects Action)
  | SetMashName
  | UpdateMashName String


update: Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    RequestReadings ->
      (model
      , getTempAction)

    LoadReadings newTemperatureReadings->
      let
        eval_readings =
          (Maybe.withDefault [] newTemperatureReadings)
      in
        case newTemperatureReadings of
          Nothing ->
            (model, Effects.none)
          Maybe.Just potato ->
            ( Model (List.append model.temperatureReadings eval_readings) model.mashName model.mashNamed
              ,Effects.none
            )
    RequestReadingsOnTime effect ->
      (model, effect)
    SetMashName ->
      (Model model.temperatureReadings model.mashName True
      ,Effects.none)

    UpdateMashName mashName ->
      (Model model.temperatureReadings mashName model.mashNamed
      ,Effects.none)


view: Signal.Address Action -> Model ->Html
view address model =

  div[class "container"]
  [ entryForm address model,
    (drawChart (extractAllTimes model) (extractAllTemps model))
  ]

extractAttributes : (a -> b) -> List a -> List b
extractAttributes attributeFunction list =
  List.map attributeFunction list


extractAllTimes : Model -> List String
extractAllTimes model =
  extractAttributes .readAt model.temperatureReadings


extractAllTemps : Model -> List Float
extractAllTemps model =
  extractAttributes .temperature model.temperatureReadings


drawChart :  Chartjs.Labels -> List Float -> Html
drawChart horizontal_axis_data vertical_axis_data =
  let
    data =
      ( horizontal_axis_data
      , [ ( "Time"
          , defStyle (rgba 220 220 220)
          , vertical_axis_data )
        ] )
  in
    div [] [fromElement <| (chart 800 600 data defaultOptions)]


entryForm: Signal.Address Action -> Model -> Html
entryForm address model =
  let
    setMashNameButton =
      if model.mashNamed
      then span[][text "Mash name set!"]
      else
        button[ class "setName", onClick address SetMashName] [text "Set Mash Name"]
  in
    div [] [
      input [
        type' "text",
        placeholder "Enter Mash Name",
        value model.mashName,
        name "phrase",
        autofocus True,
        disabled model.mashNamed,
        on "input" targetValue (\v -> Signal.message address (UpdateMashName v))
      ][],
      setMashNameButton
    ]


--Wiring
getTempAction : Effects Action
getTempAction =
    getTempFromApiTask
    |> Task.map LoadReadings
    |> Effects.task


getTempFromApiTask :Task a (Maybe (List TemperatureReading))
getTempFromApiTask  =
  Http.get jd (Http.url "http://localhost:3000/temperatures" [])
  |> Task.toMaybe


jd : Json.Decoder (List TemperatureReading)
jd =
  let tempobj =
    Json.object3
        TemperatureReading
        ("temperature" := Json.float)
        ("readAt" := Json.string)
        ("unit" := Json.string)
  in
    "temperatures" := Json.list tempobj
clock: Signal Time
clock =
  Time.every (2 * Time.second)


getTempTask : a -> Task b()
getTempTask t =
  (getTempFromApiTask)
    `Task.andThen`
    (\maybeTempReadings -> Signal.send readingsMailbox.address maybeTempReadings)


readingsMailbox : Signal.Mailbox (Maybe (List TemperatureReading))
readingsMailbox = Signal.mailbox Nothing


port periodicGet : Signal(Task() ())
port periodicGet = Signal.map getTempTask <| clock


-- Main
app :{html : Signal Html, model : Signal Model, tasks : Signal (Task Effects.Never())}
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = [Signal.map LoadReadings readingsMailbox.signal]
    }


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

main: Signal Html
main =
  app.html
