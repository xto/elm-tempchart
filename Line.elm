module Line where

import Chartjs.Line exposing (..)
import Color exposing (..)
import Signal exposing (..)
import Html exposing (..)

data : Config
data =
  ( ["January", "February", "March", "April", "May", "June", "July"]
  , [ ( "My First dataset"
      , defStyle (rgba 220 220 220)
      , [65, 59, 80, 81, 56, 55, 40]
      )]
  )


mail : Mailbox Bool
mail =
  let
    m = mailbox False
  in { m | signal = foldp (always not) False m.signal }

view : Html
view = div []
  [ fromElement <| chart 700 300 (data) defaultOptions]

main :  Html
main =
  view
