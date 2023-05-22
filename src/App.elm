module App exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, button, div, input, label, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, disabled, for, name, readonly, value)
import Html.Events exposing (onClick, onInput)
import Random
import String exposing (toInt)
import Time



-- MAIN


probability : Random.Generator Float
probability =
    Random.float 0 1


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type PaxType
    = Personal
    | Business

paxTypeToString : PaxType -> String
paxTypeToString pType =
    case pType of
        Personal -> "Personal"
        Business -> "Business"


paxType : Random.Generator PaxType
paxType =
    Random.map
        (\u ->
            if u < 0.05 then
                Business

            else
                Personal
        )
        (Random.float 0 1)


type Gender
    = Male
    | Female
    | Child

genderToString : Gender -> String
genderToString pGender =
    case pGender of
        Male -> "Male"
        Female -> "Female"
        Child -> "Child"

paxWeight : Model -> Gender -> Int
paxWeight model paxGender =
    case paxGender of
        Male -> model.weightM
        Female -> model.weightF
        Child -> model.weightK

gender : Random.Generator Gender
gender =
    Random.map
        (\x ->
            if x < 0.04 then
                Child

            else if x < 0.52 then
                Female

            else
                Male
        )
        (Random.float 0 1)


bags : Random.Generator Int
bags =
    Random.map
        (\u ->
            if u < 0.15 then
                0

            else if u < 0.75 then
                1

            else if u < 0.94 then
                2

            else if u < 0.98 then
                3

            else
                4
        )
        (Random.float 0 1)

manifest : Int -> Random.Generator (List Passenger)
manifest pax =
    Random.list pax passenger


type alias Passenger =
    { paxType : PaxType
    , gender : Gender
    , bags : Int
    }


passenger : Random.Generator Passenger
passenger =
    Random.map3
        (\t g b -> Passenger t g b)
        paxType
        gender
        bags

manifestPaxWeight : Model -> Int
manifestPaxWeight model =
    model.manifest
    |> List.map (\pax -> pax.gender)
    |> List.map (paxWeight model)
    |> List.foldl (+) 0

manifestBagWeight : Model -> Int
manifestBagWeight model =
    model.manifest
    |> List.map (\pax -> pax.bags)
    |> List.foldl (+) 0
    |> (*) model.weightB

type alias Model =
    { weightM : Int
    , weightF : Int
    , weightK : Int
    , weightB : Int
    , pax : Int
    , manifest : List Passenger
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 200 179 82 50 0 []
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdateWeightM String
    | UpdateWeightF String
    | UpdateWeightK String
    | UpdateWeightB String
    | UpdatePax String
    | GenerateManifest
    | NewManifest (List Passenger)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateWeightM weight ->
            ( { model | weightM = weight |> toInt |> Maybe.withDefault 0 }
            , Cmd.none
            )

        UpdateWeightF weight ->
            ( { model | weightF = weight |> toInt |> Maybe.withDefault 0 }
            , Cmd.none
            )

        UpdateWeightK weight ->
            ( { model | weightK = weight |> toInt |> Maybe.withDefault 0 }
            , Cmd.none
            )

        UpdateWeightB weight ->
            ( { model | weightB = weight |> toInt |> Maybe.withDefault 0 }
            , Cmd.none
            )

        UpdatePax pax ->
            ( { model | pax = pax |> toInt |> Maybe.withDefault 0 }
            , Cmd.none
            )

        GenerateManifest ->
            ( model
            , Random.generate NewManifest <| manifest model.pax
            )

        NewManifest newManifest ->
            ( { model | manifest = newManifest }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- VIEW

viewPaxData : Passenger -> Html Msg
viewPaxData pax =
    tr []
        [ td [] [ text <| paxTypeToString pax.paxType ]
        , td [] [ text <| genderToString pax.gender ]
        , td [] [ text <| String.fromInt pax.bags ]
        ]

viewManifest : Model -> List (Html Msg)
viewManifest model =
    case model.manifest of
        [] -> []
        _ ->
            [ div [ class "row justify-content-center" ]
                [ div [ class "col-auto" ]
                    [ div [ class "mb-2" ]
                        [ label [ class "form-label", for "paxWeight" ] [ text "Passenger Weight" ]
                        , input [ class "form-control", name "paxWeight", readonly True, value <| String.fromInt <| manifestPaxWeight model ] []
                        ]
                    , div [ class "mb-2" ]
                        [ label [ class "form-label", for "bagWeight" ] [ text "Baggage weight" ]
                        , input [ class "form-control", name "bagWeight", readonly True, value <| String.fromInt <| manifestBagWeight model ] []
                        ]
                    , div [ class "mb-2" ]
                        [ label [ class "form-label", for "payloadWeight" ] [ text "Payload weight" ]
                        , input [ class "form-control", name "payloadWeight", readonly True, value <| String.fromInt <| manifestPaxWeight model + manifestBagWeight model ] []
                        ]
                    ]
                , div [ class "col-auto" ]
                    [ table [ class "table" ]
                        [ thead []
                            [ th [] [ text "Type"]
                            , th [] [ text "Gender" ]
                            , th [] [ text "Bags" ] ]
                        , tbody []
                            <| List.map viewPaxData model.manifest
                        ]
                    ]
                ]
            ]

view : Model -> Html Msg
view model =
    div
        [ class "container-fluid" ]
        <| [ div [ class "h1 text-center" ] [ text "Options" ]
        , div
            [ class "row justify-content-center mb-3" ]
            [ div [ class "col-auto" ]
                [ div [ class "mb-2" ]
                    [ label [ class "form-label", for "weightM" ] [ text "Male weight" ]
                    , input [ class "form-control", name "weightM", onInput UpdateWeightM, value <| String.fromInt model.weightM ] []
                    ]
                , div [ class "mb-2" ]
                    [ label [ class "form-label", for "weightF" ] [ text "Female weight" ]
                    , input [ class "form-control", name "weightF", onInput UpdateWeightF, value <| String.fromInt model.weightF ] []
                    ]
                , div [ class "mb-2" ]
                    [ label [ class "form-label", for "weightK" ] [ text "Child weight" ]
                    , input [ class "form-control", name "weightK", onInput UpdateWeightK, value <| String.fromInt model.weightK ] []
                    ]
                , div [ class "mb-2" ]
                    [ label [ class "form-label", for "weightB" ] [ text "Checked bag weight" ]
                    , input [ class "form-control", name "weightB", onInput UpdateWeightB, value <| String.fromInt model.weightB ] []
                    ]
                ]
            , div [ class "col-auto" ]
                [ div [ class "mb-2" ]
                    [ label [ class "form-label", for "pax" ] [ text "Number of passengers" ]
                    , input [ class "form-control", name "pax", onInput UpdatePax, value <| String.fromInt model.pax ] []
                    ]
                , div [ class "mb-2" ]
                    [ label [ class "form-label", for "bags" ] [ text "Number of bags" ]
                    , input 
                        [ class "form-control"
                        , name "bags"
                        , readonly True
                        , value <| String.fromInt <| List.foldl (+) 0 <| List.map (\pax -> pax.bags) <| model.manifest
                        ] []
                    ]
                ]
            ]
        , div [ class "text-center mb-3" ]
            [ button [ class "btn btn-dark", onClick GenerateManifest ] [ text "Generate Manifest" ] ]
        ]
        ++ viewManifest model
