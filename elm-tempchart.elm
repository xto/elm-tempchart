import Http
import Json.Decode as Json exposing ((:=))
-- import Json.Encode
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


type alias BasicHttpReponse =
  {status: Int, body: String}

type alias Model = {
  temperatureReadings: List TemperatureReading
  , mashName: String
  , mashNamed: Bool
  , paused: Bool
}

temperatureReadingToJsonString: TemperatureReading -> String
temperatureReadingToJsonString temperatureReading =
  let
    temperatureString =
      "'temperature': '" ++ (toString temperatureReading.temperature) ++ "'"
    comma =
      ", "
    readAtString =
      "'readAt': '" ++temperatureReading.readAt++ "'"
    unitString =
      "'unit': '" ++temperatureReading.unit++ "'"
  in
    "{"++ temperatureString ++ comma ++ readAtString ++ comma ++ unitString ++"}"

init : (Model, Effects Action)
init =
  ((Model [] "" False False)
  , getTempAction)


--- Update

type Action
  = LoadReadings (Maybe (List TemperatureReading))
  | NoOp (Maybe BasicHttpReponse)
  | PostReadings
  | RequestReadings
  | SetMashName
  | UpdateMashName String
  | TogglePause

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
            ( Model (List.append model.temperatureReadings eval_readings) model.mashName model.mashNamed model.paused
                ,Effects.none
            )

    SetMashName ->
      if model.mashName /= "" then
        (Model model.temperatureReadings model.mashName True model.paused
        ,Effects.none)
      else
        (model, Effects.none)

    UpdateMashName mashName ->
      (Model model.temperatureReadings mashName model.mashNamed model.paused
      ,Effects.none)

    PostReadings ->
      (model, (postReadings model))

    NoOp response ->
      case response of
        Nothing ->
          (model, Effects.none)
        Maybe.Just garbage ->
          (model, Effects.none)

    TogglePause ->
      (Model model.temperatureReadings model.mashName model.mashNamed (not model.paused)
      ,Effects.none)


view: Signal.Address Action -> Model ->Html
view address model =

  div[class "container"]
  [ entryForm address model
    ,div[] [(drawChart (extractAllTimes model) (extractAllTemps model)), pauseButton address model]
    ,saveButton address model
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

saveButton: Signal.Address Action-> Model -> Html
saveButton address model =
  div [] [
    button[ class "saveButton", onClick address PostReadings] [text "Save Data"]
  ]

pauseButton: Signal.Address Action-> Model -> Html
pauseButton address model =
  div [] [
    button[ class "pauseButton", onClick pausedMailbox.address True] [text "Pause/Resume"]
  ]

--Wiring
postReadings : Model -> Effects Action
postReadings model =
  let
    potato =
      Http.multipart
        [ Http.stringData "mash_name" (model.mashName)
        , Http.stringData "temperatures" (toString(List.map temperatureReadingToJsonString model.temperatureReadings))
        ]
  in
    Http.post basicApiResponseJsonDecoder (Http.url "http://localhost:3000/temperatures" []) (potato)
    |> Task.toMaybe
    |> Task.map NoOp
    |> Effects.task

getTempAction : Effects Action
getTempAction =
    getTempFromApiTask
    |> Task.map LoadReadings
    |> Effects.task


getTempFromApiTask :Task a (Maybe (List TemperatureReading))
getTempFromApiTask  =
  Http.get temperatureReadingsJsonDecoder (Http.url "http://localhost:3000/temperatures" [])
  |> Task.toMaybe

basicApiResponseJsonDecoder : Json.Decoder BasicHttpReponse
basicApiResponseJsonDecoder =
  Json.object2
    BasicHttpReponse
    ("status" := Json.int)
    ("body" := Json.string)

temperatureReadingsJsonDecoder : Json.Decoder (List TemperatureReading)
temperatureReadingsJsonDecoder =
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


getTempTask : Bool -> Time -> Task () ()
getTempTask paused time =
  if paused
  then
    Task.succeed ()
  else
    (getTempFromApiTask)
      `Task.andThen`
      (\maybeTempReadings -> Signal.send readingsMailbox.address maybeTempReadings)

pausedMailbox: Signal.Mailbox Bool
pausedMailbox =
  let
    m = Signal.mailbox False
  in { m | signal =
    Signal.foldp (always not) False m.signal }

readingsMailbox : Signal.Mailbox (Maybe (List TemperatureReading))
readingsMailbox = Signal.mailbox Nothing


port periodicGet : Signal(Task() ())
port periodicGet = Signal.map2 getTempTask pausedMailbox.signal clock


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
