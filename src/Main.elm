module Main exposing (Flags, Model, main)

import Browser


type alias Flags =
    ()


type alias Model =
    ()


type Msg
    = NoOp


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init () =
    ( (), Cmd.none )


view : Model -> Browser.Document Msg
view () =
    { title = "Harp Transposer"
    , body = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg () =
    ( (), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions () =
    Sub.none
