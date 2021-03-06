module Chartjs.Radar
  ( chart, chart'
  , Options, defaultOptions
  , Series, Config
  , Style, defStyle, defaultStyle
  ) where

{-| API wrapper for Chartjs Radar charts

# Configure
@docs Config, Series

# Render
@docs chart, chart'

# Options
@docs Options, defaultOptions

# Style
@docs Style, defStyle, defaultStyle
-}

import Color exposing (white, rgba, rgb, Color)
import Chartjs exposing (..)
import Graphics.Element exposing (Element)

{-| Options for the Radar Chart
[Chartjs Docs](http://www.chartjs.org/docs/#line-chart-chart-options).
In most cases just use `defaultOptions`
-}
type alias Options =
  { scaleShowLine : Bool
  , angleShowLineOut : Bool
  , scaleShowLabels : Bool
  , scaleBeginAtZero : Bool
  , angleLineColor : Color
  , angleLineWidth : Float
  , pointLabelFontFamily : String
  , pointLabelFontStyle : String
  , pointLabelFontSize : Float
  , pointLabelFontColor : Color
  , pointDot : Bool
  , pointDotRadius : Float
  , pointDotStrokeWidth : Float
  , pointHitDetectionRadius : Float
  , datasetStroke : Bool
  , datasetStrokeWidth : Float
  , datasetFill : Bool
  , legendTemplate : String }

type alias OptionsRaw =
  { scaleShowLine : Bool
  , angleShowLineOut : Bool
  , scaleShowLabels : Bool
  , scaleBeginAtZero : Bool
  , angleLineColor : String
  , angleLineWidth : Float
  , pointLabelFontFamily : String
  , pointLabelFontStyle : String
  , pointLabelFontSize : Float
  , pointLabelFontColor : String
  , pointDot : Bool
  , pointDotRadius : Float
  , pointDotStrokeWidth : Float
  , pointHitDetectionRadius : Float
  , datasetStroke : Bool
  , datasetStrokeWidth : Float
  , datasetFill : Bool
  , legendTemplate : String }

decodeOptions : Options -> OptionsRaw
decodeOptions o =
  { o | angleLineColor <- showRGBA o.angleLineColor
      , pointLabelFontColor <- showRGBA o.pointLabelFontColor
      , pointLabelFontFamily <- "'" ++ o.pointLabelFontFamily ++ "'" }

{-| Codification of the default options [Chartjs Docs](http://www.chartjs.org/docs/#radar-chart-chart-options)

    chart 200 200 config defaultOptions

Pass just one option

    chart 200 200 config
      { defaultOptions | pointDot <- False }

-}
defaultOptions : Options
defaultOptions =
  { scaleShowLine = True
  , angleShowLineOut = True
  , scaleShowLabels = False
  , scaleBeginAtZero = True
  , angleLineColor = rgba 0 0 0 0.1
  , angleLineWidth = 1.0
  , pointLabelFontFamily = "Arial"
  , pointLabelFontStyle = "normal"
  , pointLabelFontSize = 10.0
  , pointLabelFontColor = rgb 102 102 102
  , pointDot = True
  , pointDotRadius = 3.0
  , pointDotStrokeWidth = 1.0
  , pointHitDetectionRadius = 20.0
  , datasetStroke = True
  , datasetStrokeWidth = 2.0
  , datasetFill = True
  , legendTemplate = "<ul class=\"<%=name.toLowerCase()%>-legend\"><% for (var i=0; i<datasets.length; i++){%><li><span style=\"background-color:<%=datasets[i].strokeColor%>\"></span><%if(datasets[i].label){%><%=datasets[i].label%><%}%></li><%}%></ul>" }


{-| Style for a data zones on the chart
[Chartjs Docs](http://www.chartjs.org/docs/#radar-chart-data-structure)
-}
type alias Style =
  { fillColor: Color
  , strokeColor: Color
  , pointColor: Color
  , pointStrokeColor: Color
  , pointHighlightFill: Color
  , pointHighlightStroke: Color }

{-| A default style for zones, a light grey affair -}
defaultStyle : Style
defaultStyle =
  defStyle (rgba 220 220 220)

{-| Convience function for making styles based on
a single color.

    myStyle = defStyle (RGBA 45 45 45)

-}
defStyle : (Float -> Color) -> Style
defStyle f =
  { fillColor = f 0.2
  , strokeColor = f 1.0
  , pointColor = f 1.0
  , pointStrokeColor = white
  , pointHighlightFill = white
  , pointHighlightStroke = f 1.0 }

{-| A Series to plot. Chartjs speak this is a dataset.
[Chartjs Docs](http://www.chartjs.org/docs/#radar-chart-data-structure) -}
type alias Series = (Label, Style, List Float)

{-| Complete data model needed for rendering
Chartjs referrs to this as simply `data` in their docs -}
type alias Config = (Labels, List Series)

type alias ConfigRaw =
  { labels : JSArray String
     , datasets : JSArray
      { label : String
      , fillColor : String
      , strokeColor : String
      , pointColor : String
      , pointStrokeColor : String
      , pointHighlightFill : String
      , pointHighlightStroke : String
      , data : JSArray Float } }

decodeConfig : Config -> ConfigRaw
decodeConfig (labels, series) = let
  decode (label, style, d) =
    { label = label
    , fillColor = showRGBA style.fillColor
    , strokeColor = showRGBA style.strokeColor
    , pointColor = showRGBA style.pointColor
    , pointStrokeColor = showRGBA style.pointStrokeColor
    , pointHighlightFill = showRGBA style.pointHighlightFill
    , pointHighlightStroke = showRGBA style.pointHighlightStroke
    , data = toArray d }
  in { labels = toArray labels
     , datasets = toArray (List.map decode series) }

{-| Create a Chartjs Radar Chart in an Element

    chart height width myConfig defaultOptions

-}
chart : Int -> Int -> Config -> Options -> Element
chart w h c o = chartRaw "Radar" w h (decodeConfig c) (decodeOptions o)

{-| Same as `chart` but default options are assumed -}
chart' : Int -> Int -> Config -> Element
chart' w h c = chart w h c defaultOptions
