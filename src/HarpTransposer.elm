module HarpTransposer exposing (Content, Flags, Model, Msg, Position, main)

import AssocList as Dict exposing (Dict)
import Browser
import Css.Global
import Html.Styled as Html exposing (Attribute, Html, button, div, h1, h2, h3, input, label, li, option, select, span, text, textarea, ul)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events as Evt
import Html.Styled.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Music
import Tailwind.Breakpoints as Br
import Tailwind.Theme as Theme
import Tailwind.Utilities as Tw


type alias Flags =
    ()


type Position
    = FirstPos
    | SecondPos
    | ThirdPos


allPositions : List ( String, Position )
allPositions =
    [ ( "1st", FirstPos ), ( "2nd", SecondPos ), ( "3rd", ThirdPos ) ]


toMusicPosition : Position -> Music.Position
toMusicPosition position =
    case position of
        FirstPos ->
            Music.firstPosition

        SecondPos ->
            Music.secondPosition

        ThirdPos ->
            Music.thirdPosition


type alias Model =
    { from : Position
    , to : Position
    , content : Content
    , edits : Dict Int Music.TransposedNote
    , globalEdits : Dict Music.HarmonicaNote Music.HarmonicaNote
    , editing : Maybe ( Int, Bool )
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
    | Edit (Maybe Int)
    | UpdateTransposedNote Int Music.TransposedNote Bool
    | ToggleGlobalEdit Bool
    | RemoveGlobal Music.HarmonicaNote
    | ClearLocalEdits


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
      , edits = Dict.empty
      , globalEdits = Dict.empty
      , editing = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewContent rawLick ->
            ( { model | content = contentFromRawLick rawLick }, Cmd.none )

        ChangeFrom newFrom ->
            ( { model
                | from = newFrom
                , edits = Dict.empty
                , globalEdits = Dict.empty
              }
            , Cmd.none
            )

        ChangeTo newTo ->
            ( { model
                | to = newTo
                , edits = Dict.empty
                , globalEdits = Dict.empty
              }
            , Cmd.none
            )

        Edit (Just index) ->
            ( { model | editing = Just ( index, False ) }, Cmd.none )

        Edit Nothing ->
            ( { model | editing = Nothing }, Cmd.none )

        ToggleGlobalEdit bool ->
            ( { model
                | editing =
                    model.editing
                        |> Maybe.map
                            (\( index, _ ) ->
                                ( index, bool )
                            )
              }
            , Cmd.none
            )

        UpdateTransposedNote index updatedNote isGlobal ->
            if isGlobal then
                ( { model
                    | editing = Nothing
                    , globalEdits =
                        case updatedNote.selected of
                            Just selected ->
                                model.globalEdits |> Dict.insert updatedNote.original selected

                            Nothing ->
                                model.globalEdits
                  }
                , Cmd.none
                )

            else
                ( { model
                    | editing = Nothing
                    , edits = model.edits |> Dict.insert index updatedNote
                  }
                , Cmd.none
                )

        ClearLocalEdits ->
            ( { model | edits = Dict.empty }, Cmd.none )

        RemoveGlobal note ->
            ( { model
                | globalEdits =
                    model.globalEdits
                        |> Dict.remove note
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


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
                    , Tw.flex
                    , Tw.flex_wrap
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
        , Tw.flex
        , Tw.flex_col
        , Tw.snap_always
        , Tw.snap_center
        , Tw.justify_center
        , Tw.items_center
        , Tw.w_screen
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
        , css
            [ Br.md
                [ Tw.w_1over2
                ]
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
        , css
            [ Br.md
                [ Tw.w_1over2
                ]
            ]
        ]
        [ div
            [ css
                [ Tw.flex
                , Tw.flex_col
                , Tw.flex_grow
                , Tw.w_full
                , Tw.h_full
                , Tw.bg_color Theme.white
                , Tw.bg_opacity_50
                , Tw.p_5
                , Tw.space_y_5
                ]
            ]
            [ h2 [ css [ Tw.text_color Theme.gray_800, Tw.text_2xl ] ] [ text "Output" ]
            , div [ css [] ]
                [ label [ css [ Tw.flex, Tw.items_center, Tw.space_x_2 ] ]
                    [ span [] [ text "Targeted position" ]
                    , positionSelector to ChangeTo
                    ]
                ]
            , viewEditsControl model
            , viewTransposedLick model
            ]
        ]


viewEditsControl : Model -> Html Msg
viewEditsControl { globalEdits, edits } =
    Html.viewIf (not (Dict.isEmpty globalEdits && Dict.isEmpty edits)) <|
        div [ css [ Tw.flex, Tw.flex_col, Tw.space_y_4, Tw.text_sm ] ] <|
            [ Html.viewIf (not <| Dict.isEmpty globalEdits) <|
                div [ css [ Tw.bg_color Theme.red_50, Tw.p_2, Tw.rounded_xl ] ]
                    [ h3 [ css [ Tw.text_lg ] ] [ text "global changes" ]
                    , ul [ css [ Tw.flex, Tw.flex_col, Tw.space_y_1, Tw.max_w_max, Tw.list_disc ] ]
                        (globalEdits
                            |> Dict.toList
                            |> List.map
                                (\( from, to ) ->
                                    li [ css [ Tw.flex, Tw.w_full, Tw.justify_between, Tw.items_center, Tw.space_x_2, Tw.pl_4 ] ]
                                        [ span [] [ text <| Music.noteToString from ++ " → " ++ Music.noteToString to ]
                                        , button
                                            [ css [ Tw.text_color Theme.red_500, Tw.bg_color Theme.red_50, Tw.p_1, Tw.shadow_md, Tw.rounded_xl ]
                                            , Evt.onClick <| RemoveGlobal from
                                            ]
                                            [ text "🗑" ]
                                        ]
                                )
                        )
                    ]
            , Html.viewIf (not <| Dict.isEmpty edits) <|
                div []
                    [ button
                        [ css
                            [ Tw.flex
                            , Tw.space_x_2
                            , Tw.p_3
                            , Tw.mr_auto
                            , Tw.text_color Theme.red_500
                            , Tw.bg_color Theme.red_50
                            , Tw.shadow_md
                            , Tw.rounded_xl
                            ]
                        , Evt.onClick ClearLocalEdits
                        ]
                        [ text "🗑   Clear local edits" ]
                    ]
            ]


applyEdits : Dict Music.HarmonicaNote Music.HarmonicaNote -> Dict Int Music.TransposedNote -> Music.TransposedLick -> Music.TransposedLick
applyEdits globals edits =
    List.indexedMap
        (\index transposedElement ->
            case transposedElement of
                Music.NoteOptions transposedNote ->
                    case ( Dict.get index edits, Dict.get transposedNote.original globals ) of
                        ( Just edit, Just global ) ->
                            if transposedNote.original == edit.original then
                                Music.NoteOptions edit

                            else
                                Music.NoteOptions { transposedNote | selected = Just global }

                        ( Just edit, _ ) ->
                            if transposedNote.original == edit.original then
                                Music.NoteOptions edit

                            else
                                Music.NoteOptions transposedNote

                        ( _, Just global ) ->
                            Music.NoteOptions { transposedNote | selected = Just global }

                        _ ->
                            Music.NoteOptions transposedNote

                _ ->
                    transposedElement
        )


viewTransposedLick : Model -> Html Msg
viewTransposedLick { content, from, to, edits, globalEdits, editing } =
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
                        |> applyEdits globalEdits edits
            in
            div [ css [ Tw.bg_color Theme.white, Tw.whitespace_pre, Tw.p_5, Tw.flex_grow, Tw.h_0 ] ]
                [ div [ css [ Tw.max_h_full, Tw.min_h_0, Tw.overflow_y_scroll ] ]
                    [ div [ css [ Tw.overflow_y_scroll ] ]
                        (candidateLick
                            |> List.indexedMap
                                (\index elm ->
                                    case elm of
                                        Music.Annotation_ annotation ->
                                            span [] [ text annotation ]

                                        Music.NoteOptions note ->
                                            viewTransposedNote { index = index, editing = editing } note
                                )
                        )
                    ]
                ]


viewTransposedNote : { index : Int, editing : Maybe ( Int, Bool ) } -> Music.TransposedNote -> Html Msg
viewTransposedNote { index, editing } ({ original, options, selected } as transposedNote) =
    div
        [ css [ Tw.relative, Tw.text_color Theme.violet_700, Tw.inline_block, Tw.cursor_pointer ]
        ]
    <|
        (if Just index == Maybe.map Tuple.first editing then
            [ div
                [ css
                    [ Tw.fixed
                    , Tw.top_0
                    , Tw.bottom_0
                    , Tw.left_0
                    , Tw.right_0
                    , Tw.z_10
                    ]
                , Evt.onClick <| Edit Nothing
                ]
                []
            , let
                isGlobal : Bool
                isGlobal =
                    editing |> Maybe.unwrap False Tuple.second
              in
              div
                [ css
                    [ Tw.absolute
                    , Tw.top_full
                    , Tw.start_0
                    , Tw.bg_color Theme.fuchsia_100
                    , Tw.p_2
                    , Tw.rounded_xl
                    , Tw.rounded_tl_none
                    , Tw.z_20
                    , Tw.flex
                    , Tw.flex_col
                    ]
                ]
                [ span [] [ text "Choose:" ]
                , span [ css [ Tw.text_sm, Tw.pb_2 ] ] [ text <| "(original " ++ Music.noteToString original ++ ")" ]
                , ul [ css [ Tw.w_full, Tw.space_y_1, Tw.text_center ] ]
                    (options
                        |> List.map
                            (\note ->
                                li
                                    [ css [ Tw.w_full, Tw.bg_color Theme.violet_50, Tw.rounded_xl ]
                                    , Evt.onClick <|
                                        UpdateTransposedNote index
                                            { transposedNote
                                                | selected = Just note
                                            }
                                            isGlobal
                                    ]
                                    [ text <| Music.noteToString note ]
                            )
                    )
                , div [ css [ Tw.flex, Tw.space_x_1, Tw.text_sm ] ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.checked isGlobal
                        , Evt.onCheck ToggleGlobalEdit
                        ]
                        []
                    , span [] [ text "Apply globally" ]
                    ]
                ]
            ]

         else
            []
        )
            ++ [ div
                    [ css [ Tw.bg_color Theme.violet_50, Tw.p_1, Tw.rounded_xl ]
                    , Evt.onClick <| Edit <| Just index
                    ]
                    [ selected
                        |> Maybe.map Just
                        |> Maybe.withDefault
                            (options
                                |> List.head
                            )
                        |> Maybe.unwrap "‽" Music.noteToString
                        |> text
                    ]
               ]
