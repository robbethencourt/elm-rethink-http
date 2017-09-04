module Main exposing (..)

import Html exposing (Html, text, div, img, input)
import Html.Attributes exposing (src, type_, value, disabled)
import Html.Events exposing (onClick)
import Http as Http
import App as App
import Profile as Profile


---- UPDATE ----


type Msg
    = FetchUser App.FetchCap
    | RequestReceived App.Msg



-- | ProfileRequest


update : Msg -> App.Model -> ( App.Model, Cmd Msg )
update msg model =
    case msg of
        FetchUser cap ->
            App.fetch RequestReceived cap

        RequestReceived msg_ ->
            ( App.process msg_, Cmd.none )


httpErrorString : Http.Error -> String -> String
httpErrorString errorMessage customMessage =
    case errorMessage of
        Http.BadUrl errMsg ->
            customMessage ++ errMsg

        Http.Timeout ->
            customMessage ++ "I got a Timeout error"

        Http.NetworkError ->
            customMessage ++ "I got a Network Error"

        Http.BadStatus response ->
            customMessage ++ "Bad Http Status: " ++ toString response.status.message

        Http.BadPayload errMsg response ->
            customMessage
                ++ "Bad Http Payload: "
                ++ toString errMsg
                ++ " ("
                ++ toString response.status.code
                ++ ")"



---- VIEW ----


view : App.Model -> Html Msg
view model =
    case model of
        App.Initial _ cap ->
            view_
                "Info goes here. Go head, do a fetch. I dare you."
                (FetchUser cap |> Just)
                model

        App.Fetching _ ->
            view_
                "Fetching..."
                Nothing
                model

        App.FetchError _ err cap ->
            view_
                (httpErrorString err "Fetch failed!")
                (FetchUser cap |> Just)
                model

        App.FetchSuccess _ profile ->
            view_
                (Profile.apply (\{ username } -> "Success! " ++ username) profile)
                Nothing
                model


view_ : String -> Maybe Msg -> App.Model -> Html Msg
view_ message buttonMsg model =
    let
        buttonAttrs =
            Maybe.map (\msg -> List.singleton (onClick msg)) buttonMsg
                |> Maybe.withDefault [ disabled True ]
                |> List.append
                    [ type_ "button"
                    , value "Get User!"
                    ]
    in
        div []
            [ div []
                [ input buttonAttrs []
                ]
            , div []
                [ text message ]
            , div [] [ text (toString model) ]
            ]



---- PROGRAM ----


{-| Sets up the Elm app, but notice the init field is not present. That's because it's provided by App.program because it's in control of the state changes, including the initial state.
-}
main : Program Never App.Model Msg
main =
    App.program
        { view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
