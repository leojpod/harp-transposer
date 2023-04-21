module Main exposing (Flags, Model, main)

import Browser
import Css.Global
import Html.Styled as Html exposing (Attribute, Html, div, h1, h2, h3, text, textarea)
import Html.Styled.Attributes exposing (css)
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


type alias Flags =
    ()


type alias Model =



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
view model =
    { title = "Harp Transposer"
    , body =
        [ Html.toUnstyled <|
            div
                [ css [ Tw.snap_y, Tw.snap_mandatory, Tw.overflow_y_auto, Tw.h_screen, Tw.w_screen ]
                ]
                [ Css.Global.global Tw.globalStyles
                , introPage
                , inputPage model
                , outputPage model
                ]
        ]
    }


pageAttributes : Attribute Msg
pageAttributes =
    css
        [ Tw.bg_gradient_to_br
        , Tw.h_screen
        , Tw.w_screen
        , Tw.flex
        , Tw.flex_col
        , Tw.snap_always
        , Tw.snap_center
        , Tw.justify_center
        , Tw.items_center
        ]


introPage : Html Msg
introPage =
    div
        [ pageAttributes
        , css
            [ Tw.from_color Theme.violet_400
            , Tw.to_color Theme.violet_50
            , Tw.space_y_10
            ]
        ]
        [ h1 [ css [ Tw.text_7xl, Tw.text_color Theme.gray_50 ] ] [ text "Harp Transposer" ]
        , h2 [ css [ Tw.text_7xl, Tw.text_color Theme.gray_500 ] ] [ text "This should help you take a riff in one position/octave and move it to another position/octave" ]
        , h3 [ css [ Tw.text_3xl, Tw.text_color Theme.gray_600 ] ] [ text "scroll down to get started" ]
        ]


inputPage : Model -> Html Msg
inputPage _ =
    div
        [ pageAttributes
        , css
            [ Tw.from_color Theme.green_400
            , Tw.to_color Theme.green_50
            , Tw.p_10
            ]
        ]
        [ div
            [ css
                [ Tw.w_full
                , Tw.flex
                , Tw.flex_col
                , Tw.bg_color Theme.white
                , Tw.bg_opacity_75
                , Tw.flex_grow
                , Tw.space_y_5
                , Tw.p_5
                ]
            ]
            [ h2 [ css [ Tw.text_color Theme.gray_800, Tw.text_2xl ] ] [ text "Input" ]
            , textarea [ css [ Tw.flex_grow ] ] []
            ]
        ]


outputPage : Model -> Html Msg
outputPage _ =
    div
        [ pageAttributes
        , css
            [ Tw.from_color Theme.red_400
            , Tw.to_color Theme.red_50
            ]
        ]
        [ h1 [] [ text "There will be an output here" ] ]


update : Msg -> Model -> ( Model, Cmd Msg )
update _ () =
    ( (), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions () =
    Sub.none
