module Main exposing (..)

import Html exposing (Html, text, div, img, input)
import Html.Attributes exposing (src, type_, value)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as JDP exposing (decode, required)


---- MODEL ----


type alias Model =
    { profile : Profile
    , page : Page
    , theError : Maybe String
    }


type ExternalResource
    = NotRequested
    | Loading
    | UserReceived (Result Http.Error Profile)


type Page
    = LoadingView
    | TheView


initProfile : Profile
initProfile =
    { username = "" }


type alias Profile =
    { username : String }


init : ( Model, Cmd Msg )
init =
    ( { profile = initProfile
      , page = TheView
      , theError = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = FetchUser
    | RequestReceived (Result Http.Error Profile)



-- | ProfileRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchUser ->
            ( model, fetchUser )

        RequestReceived result ->
            case result of
                Ok bijan ->
                    ( model, Cmd.none )

                Err error ->
                    ( { model | theError = Just (httpErrorString error "Woops! ") }, Cmd.none )


fetchUser : Cmd Msg
fetchUser =
    let
        url =
            "https://www.codeschool.com/users/bijanbwb.json"

        request =
            Http.get url decodeUser
    in
        Http.send RequestReceived request


decodeUser : Decode.Decoder Profile
decodeUser =
    Decode.at [ "user" ] decodeMore


decodeMore : Decode.Decoder Profile
decodeMore =
    decode Profile
        |> JDP.required "username" Decode.string


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


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input
                [ type_ "button"
                , value "Get User!"
                , onClick FetchUser
                ]
                []
            ]
        , div []
            [ text "this is where the info goes." ]
        , div [] [ text (toString model) ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
