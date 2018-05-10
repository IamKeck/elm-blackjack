module Main exposing (..)

import Html exposing (program)
import Model
import Update
import View


init : ( Model.Model, Cmd Model.Msg )
init =
    (Model.initialModel ! [])


subscriptions : Model.Model -> Sub Model.Msg
subscriptions _ =
    Sub.none


main =
    program { init = init, view = View.view, update = Update.update, subscriptions = subscriptions }
