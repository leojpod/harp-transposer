module Main exposing (Flags, Model, main)

import Browser
import Css.Global
import Html.Styled as Html exposing (Attribute, Html, div, h1, h2, h3, label, option, select, span, text, textarea)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Evt
import List.Extra as List
import Maybe.Extra as Maybe
import Music
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


type alias Flags =
    ()


type Position
    = FirstPos
    | SecondPos


allPositions : List ( String, Position )
allPositions =
    [ ( "1st", FirstPos ), ( "2nd", SecondPos ) ]


toMusicPosition : Position -> Music.Position
toMusicPosition position =
    case position of
        FirstPos ->
            Music.firstPosition

        SecondPos ->
            Music.secondPosition


type alias Model =
    { from : Position
    , to : Position
    , content : Content
    }


type Content
    = InvalidLick { reason : String, rawLick : String }
    | ValidLick { rawLick : String, lick : Music.WrittenLick }


getRawLick : Content -> String
getRawLick content =
    case content of
        InvalidLick { rawLick } ->
            rawLick

        ValidLick { rawLick } ->
            rawLick


contentFromRawLick : String -> Content
contentFromRawLick rawLick =
    case Music.parseLick rawLick of
        Err reason ->
            InvalidLick { reason = reason, rawLick = rawLick }

        Ok lick ->
            ValidLick { rawLick = rawLick, lick = lick }


type Msg
    = NewContent String
    | ChangeFrom Position
    | ChangeTo Position
    | NoOp


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
    ( { from = FirstPos
      , to = SecondPos
      , content = contentFromRawLick ""
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Harp Transposer"
    , body =
        [ Html.toUnstyled <|
            div
                [ css
                    [ Tw.snap_y
                    , Tw.snap_mandatory
                    , Tw.overflow_y_auto
                    , Tw.h_screen
                    , Tw.w_screen
                    ]
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
            , Tw.text_center
            ]
        ]
        [ h1 [ css [ Tw.text_7xl, Tw.text_color Theme.gray_50 ] ] [ text "Harp Transposer" ]
        , h2
            [ css
                [ Tw.text_3xl
                , Tw.text_color Theme.gray_500
                , Tw.text_center
                ]
            ]
            [ text "This should help you take a riff in one position/octave and move it to another position/octave" ]
        , h3
            [ css
                [ Tw.text_2xl
                , Tw.text_color Theme.gray_600
                ]
            ]
            [ text "scroll down to get started" ]
        ]


inputPage : Model -> Html Msg
inputPage { content, from } =
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
            , div [ css [] ]
                [ label [ css [ Tw.flex, Tw.items_center, Tw.space_x_2 ] ]
                    [ span [] [ text "Position of the lick" ]
                    , positionSelector from ChangeFrom
                    ]
                ]
            , textarea
                [ css [ Tw.flex_grow, Tw.p_5 ]
                , Attr.placeholder "Put your lick to transpose here"
                , Attr.value <| getRawLick content
                , Evt.onInput NewContent
                ]
                []
            ]
        ]


positionSelector : Position -> (Position -> Msg) -> Html Msg
positionSelector selectedPosition toMsg =
    let
        mapper : String -> Msg
        mapper name =
            allPositions
                |> List.find (\( posName, _ ) -> posName == name)
                |> Maybe.unwrap selectedPosition Tuple.second
                |> toMsg
    in
    select
        [ Evt.onInput mapper
        , css [ Tw.bg_color Theme.white, Tw.py_2, Tw.px_4 ]
        ]
    <|
        option [ Attr.disabled True ] [ text "Position" ]
            :: (allPositions
                    |> List.map
                        (\( posName, position ) ->
                            option
                                [ Attr.value posName
                                , Attr.selected <| position == selectedPosition
                                ]
                                [ text posName ]
                        )
               )


outputPage : Model -> Html Msg
outputPage ({ to } as model) =
    div
        [ pageAttributes
        , css
            [ Tw.from_color Theme.red_400
            , Tw.to_color Theme.red_50
            , Tw.p_10
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.flex_grow
                , Tw.w_full
                , Tw.bg_color Theme.white
                , Tw.bg_opacity_50
                , Tw.p_5
                , Tw.space_y_5
                ]
            ]
            [ h2 [ css [ Tw.text_color Theme.gray_800, Tw.text_2xl ] ] [ text "Output" ]
            , div [ css [] ]
                [ label [ css [ Tw.flex, Tw.items_center, Tw.space_x_2 ] ]
                    [ span [] [ text "Targeted position" ] ]
                , positionSelector to ChangeTo
                ]
            , viewTransposedLick model
            ]
        ]


viewTransposedLick : Model -> Html Msg
viewTransposedLick { content, from, to } =
    case content of
        InvalidLick _ ->
            div [ css [] ]
                [ span [ css [ Tw.m_auto, Tw.text_2xl ] ] [ text "Check the lick, it seems that something is wrong" ]
                ]

        ValidLick { lick } ->
            let
                candidateLick : Music.TransposedLick
                candidateLick =
                    lick
                        |> Music.transpose (from |> toMusicPosition) (to |> toMusicPosition)
            in
            div [ css [ Tw.bg_color Theme.white, Tw.whitespace_pre, Tw.p_5, Tw.flex_grow ] ]
                (candidateLick
                    |> List.map
                        (\elm ->
                            case elm of
                                Music.Annotation_ annotation ->
                                    span [] [ text annotation ]

                                Music.NoteOptions { options } ->
                                    span [ css [ Tw.text_color Theme.violet_700 ] ]
                                        [ options
                                            |> List.head
                                            |> Maybe.unwrap "" Music.noteToString
                                            |> text
                                        ]
                        )
                )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewContent rawLick ->
            ( { model | content = contentFromRawLick rawLick }, Cmd.none )

        ChangeFrom newFrom ->
            ( { model | from = newFrom }, Cmd.none )

        ChangeTo newTo ->
            ( { model | to = newTo }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
