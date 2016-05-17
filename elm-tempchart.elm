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
import Date
import Date.Format

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
      "\"temperature\": \"" ++ (toString temperatureReading.temperature) ++ "\""
    comma =
      ", "
    readAtString =
      "\"readAt\": \"" ++(Date.Format.format "%k:%M:%S" (Date.fromString temperatureReading.readAt |> Result.withDefault (Date.fromTime 0)))++ "\""
    unitString =
      "\"unit\": \"" ++temperatureReading.unit++ "\""
  in
    "{"++ temperatureString ++ comma ++ readAtString ++ comma ++ unitString ++"}"

init : (Model, Effects Action)
init =
  ((Model [] "" False False)
  , getTempAction)


--- Update

type Action
  = HandleHttpResponse (Maybe BasicHttpReponse)
  | LoadReadings (Maybe (List TemperatureReading))
  | LoadMash
  | NoOp (Maybe ())
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
      , getTempAction )

    LoadReadings newTemperatureReadings->
      let
        eval_readings =
          (Maybe.withDefault [] newTemperatureReadings)
      in
        case newTemperatureReadings of
          Nothing ->
            (model, Effects.none)
          Maybe.Just postData ->
            ( Model (List.append model.temperatureReadings eval_readings) model.mashName model.mashNamed model.paused
                ,Effects.none
            )
    LoadMash ->
      (Model [] model.mashName model.mashNamed model.paused
      , (getMashAction model.mashName))

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

    HandleHttpResponse response ->
      case response of
        Nothing ->
          (model, Effects.none)
        Maybe.Just garbage ->
          (model, Effects.none)

    NoOp a ->
      (model, Effects.none)

    TogglePause ->
      (Model model.temperatureReadings model.mashName model.mashNamed (not model.paused)
      ,signalPause )

signalPause : Effects Action
signalPause =
  (Signal.send pausedMailbox.address True)
  |> Task.toMaybe
  |> Task.map NoOp
  |> Effects.task

view: Signal.Address Action -> Model ->Html
view address model =
  div[class "container"]
  [ entryForm address model
    ,div[class "flex-cell graph"] [(drawChart (extractAllTimes model) (extractAllTemps model))]
    ,div[class "flex-cell"][
      div[class "btn-group"][saveButton address model, pauseButton address model, loadButton address model]
    ]
  ]

extractAttributes : (a -> b) -> List a -> List b
extractAttributes attributeFunction list =
  List.map attributeFunction list

reformatDate : String -> String
reformatDate dateString =
  Date.Format.format "%k:%M:%S" (Date.fromString dateString |> Result.withDefault (Date.fromTime 0))

extractAllTimes : Model -> List String
extractAllTimes model =
  let
    listTimes=
      extractAttributes .readAt model.temperatureReadings
  in
    List.map reformatDate listTimes



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
        button[ class "btn btn-primary btn-xs setName", onClick address SetMashName] [text "Set Mash Name"]
  in
    div [class "flex-cell flex-cell--single"] [
      div[class "input-group"][input [
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
    ]


saveButton: Signal.Address Action-> Model -> Html
saveButton address model =
  button[ class "btn btn-primary saveButton", onClick address PostReadings] [text "Save Data"]

loadButton : Signal.Address Action -> Model -> Html
loadButton address model =
  button[ class "btn btn-primary loadButton", onClick address LoadMash] [text "Load Mash Data"]

pauseButton: Signal.Address Action -> Model -> Html
pauseButton address model =
  let
    label =
      if model.paused then
        "Resume"
      else
        "Pause"
  in
    -- div [] [
    button[ class "btn btn-primary pauseButton", onClick address TogglePause] [text label]
    -- ]

--Wiring
postReadings : Model -> Effects Action
postReadings model =
  let
    postData =
      Http.multipart
        [ Http.stringData "mash_name" (model.mashName)
        , Http.stringData "temperatures" (toString(List.map temperatureReadingToJsonString model.temperatureReadings))
        ]
  in
    Http.post basicApiResponseJsonDecoder (Http.url "http://localhost:3000/temperatures" []) (postData)
    |> Task.toMaybe
    |> Task.map HandleHttpResponse
    |> Effects.task

getTempAction : Effects Action
getTempAction =
    getTempFromApiTask
    |> Task.map LoadReadings
    |> Effects.task

getMashAction : String -> Effects Action
getMashAction mashName =
  getMashFromApiTask mashName
  |> Task.map LoadReadings
  |> Effects.task

getMashFromApiTask : String -> Task a (Maybe (List TemperatureReading))
getMashFromApiTask mashName =
  Http.get temperatureReadingsJsonDecoder (Http.url ("http://localhost:3000/mashes/" ++ mashName) [] )
  |> Task.toMaybe

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
