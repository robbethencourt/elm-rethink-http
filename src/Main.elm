module Main exposing (..)

import Html exposing (Html, text, div, img, input)
import Html.Attributes exposing (src, type_, value)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as JDP exposing (decode, required)


---- MODEL ----


type alias Model =
    { repo : List Repo
    , page : Page
    , errorMessage : Maybe String
    }


type ExternalResource
    = NotRequested
    | Loading
    | UserReceived (Result Http.Error Repo)


type Page
    = LoadingView
    | TheView



-- initRepo : Repo
-- initRepo =
--     { name = ""
--     , private = False
--     , url = ""
--     , created_at = ""
--     , stargazers_count = 0
--     , open_issues_count = 0 }


type alias Repo =
    { name : String
    , private : Bool
    , url : String
    , created_at : String
    , stargazers_count : Int
    , open_issues_count : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { repo = []
      , page = TheView
      , errorMessage = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = FetchRepos
    | RequestReceived (Result Http.Error Repo)



-- | RepoRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchRepos ->
            ( model, fetchRepos )

        RequestReceived result ->
            case result of
                Ok repos ->
                    ( model, Cmd.none )

                Err error ->
                    ( { model | errorMessage = Just (httpErrorString error "Woops! ") }, Cmd.none )


fetchRepos : Cmd Msg
fetchRepos =
    let
        language =
            "elm"

        date =
            "2017-09-01"

        url =
            "https://api.github.com/search/repositories?q=language:"
                ++ language
                ++ "+created:>="
                ++ date

        request =
            Http.get url decodeRepo
    in
        Http.send RequestReceived request


decodeRepo : Decode.Decoder Repo
decodeRepo =
    decode Repo
        |> JDP.required "name" Decode.string
        |> JDP.required "private" Decode.bool
        |> JDP.required "url" Decode.string
        |> JDP.required "created_at" Decode.string
        |> JDP.required "stargazers_count" Decode.int
        |> JDP.required "open_issues_count" Decode.int


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
                , value "Get Repos!"
                , onClick FetchRepos
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
