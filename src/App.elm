module App exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)
import Time

-- Add comment to bump github

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { clicks : Int
    , cursors : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 0
    , Cmd.none
    )


-- UPDATE


type Msg
    = Tick Time.Posix
    | Click
    | BuyCursor


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | clicks = model.clicks + (round <| 0.1 * toFloat model.cursors) }
            , Cmd.none
            )
        Click ->
            ( { model | clicks = model.clicks + 1}
            , Cmd.none
            )
        BuyCursor ->
            if model.clicks >= 15 then
                ( { model | cursors = model.cursors + 1, clicks = model.clicks - 15 }
                , Cmd.none
                )
            else
                ( model, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "container-fluid" ]
        [ div [ class "h1 text-center" ] [ text <| String.fromInt model.clicks ]
        , div
            [ class "row justify-content-center" ]
            [ div [ class "col-auto" ] [ button [ class "btn btn-primary", onClick Click ] [ text "Click" ] ]
            , div [ class "col-auto" ] [ button [ class "btn btn-primary", onClick BuyCursor, disabled <| model.clicks < 15 ] [ text "Cursor" ] ]
            ]]