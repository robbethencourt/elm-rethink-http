module Main exposing (..)

import Loading
import Html exposing (Html, text, div, button, input, h1, label, select, option, table, thead, tr, th, tbody, td, a, p)
import Html.Attributes exposing (src, type_, value, class, style, href)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as JDP exposing (decode, required)
import Task
import Date


---- MODEL ----


type alias Model =
    { isLoading : Bool
    , resultsReceived : Bool
    , languageInput : String
    , dateInput : String
    , repos : List Repo
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


languages : List String
languages =
    [ "Elm", "Purescript", "Idris", "ClojureScript", "Fable", "GHCJS - Haskell", "ElixirScript", "Js_of_ocamal", "BuckleScript", "Reason", "Scala.js", "LiveScript", "Quack", "ion", "RamdaScript" ]


type alias Repo =
    { name : String
    , private : Bool
    , html_url : String
    , created_at : String
    , stargazers_count : Int
    , open_issues_count : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { isLoading = False
      , resultsReceived = False
      , languageInput = "Elm"
      , dateInput = ""
      , repos = []
      , page = TheView
      , errorMessage = Nothing
      }
    , setTime
    )



---- UPDATE ----


type Msg
    = SetDate Date.Date
    | SelectLanguage String
    | ChangeDate String
    | FetchRepos
    | RequestReceived (Result Http.Error (List Repo))



-- | RepoRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetDate date ->
            ( { model
                | dateInput = convertDateForHtmlDatepicker date
              }
            , Cmd.none
            )

        SelectLanguage language ->
            ( { model
                | languageInput = language
              }
            , Cmd.none
            )

        ChangeDate newDate ->
            ( { model
                | dateInput = newDate
              }
            , Cmd.none
            )

        FetchRepos ->
            ( { model
                | isLoading = True
                , resultsReceived = False
              }
            , fetchRepos model
            )

        RequestReceived result ->
            case result of
                Ok repos ->
                    ( { model
                        | errorMessage = Nothing
                        , repos = repos
                        , isLoading = False
                        , resultsReceived = True
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | errorMessage = Just (httpErrorString error "Woops! ")
                        , isLoading = False
                        , resultsReceived = True
                      }
                    , Cmd.none
                    )


setTime : Cmd Msg
setTime =
    Task.perform SetDate Date.now


convertDateForHtmlDatepicker : Date.Date -> String
convertDateForHtmlDatepicker date =
    toString (Date.year date)
        ++ "-"
        ++ formatDateWithZero (toString (convertMonth (Date.month date)))
        ++ "-"
        ++ formatDateWithZero (toString (Date.day date))


formatDateWithZero : String -> String
formatDateWithZero date =
    let
        intDate =
            Result.withDefault 0 (String.toInt date)
    in
        if intDate < 10 then
            "0" ++ date
        else
            date


convertMonth : Date.Month -> Int
convertMonth month =
    case month of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12


fetchRepos : Model -> Cmd Msg
fetchRepos { languageInput, dateInput } =
    let
        language =
            languageInput

        date =
            dateInput

        url =
            "https://api.github.com/search/repositories?q="
                ++ language
                ++ "+created:>="
                ++ date

        request =
            Http.get url decodeRequest
    in
        Http.send RequestReceived request


decodeRequest : Decode.Decoder (List Repo)
decodeRequest =
    Decode.at [ "items" ] (Decode.list decodeRepo)


decodeRepo : Decode.Decoder Repo
decodeRepo =
    decode Repo
        |> JDP.required "name" Decode.string
        |> JDP.required "private" Decode.bool
        |> JDP.required "html_url" Decode.string
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
    div [ style [ ( "margin", "20px 10%" ) ] ]
        [ div [ class "row" ]
            [ div [ class "col" ]
                [ h1 [ class "text-center" ] [ text "FP to JS Github Repos" ] ]
            ]
        , div [ class "row" ]
            [ div [ class "col" ]
                [ label [] [ text "Language" ]
                , select
                    [ onInput SelectLanguage ]
                    (List.map languageOption languages)
                ]
            , div [ class "col" ]
                [ label [] [ text "Date" ]
                , input
                    [ type_ "date"
                    , value model.dateInput
                    , onInput ChangeDate
                    ]
                    []
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col" ]
                [ button
                    [ onClick FetchRepos
                    ]
                    [ text "Get The Repos!" ]
                ]
            , div [ class "col" ] []
            ]
        , div [ class "row" ]
            [ reposTable model.repos ]
        , if model.isLoading then
            div [ class "row" ]
                [ div [ class "col text-center" ]
                    [ Loading.loadingAnimation ]
                ]
          else
            div [] []
        , if model.errorMessage /= Nothing then
            div [ class "row" ]
                [ div [ class "col" ]
                    [ p [ class "error-message" ] [ text (Maybe.withDefault "" model.errorMessage) ] ]
                ]
          else
            div [] []
        , if List.length model.repos == 0 && model.resultsReceived then
            div [ class "row" ]
                [ div [ class "col" ]
                    [ p [ class "message" ] [ text "No repos found. Try searching from an earlier date, or get motivated and create a repo on github." ] ]
                ]
          else
            div [] []
        ]


languageOption : String -> Html Msg
languageOption language =
    option [ value language ] [ text language ]


reposTable : List Repo -> Html Msg
reposTable repos =
    repos
        |> List.map reposTableBody
        |> tbody []
        |> appendTableHeader reposTableHeader
        |> table [ class "table" ]


reposTableHeader : Html Msg
reposTableHeader =
    thead []
        [ tr []
            [ th [] [ text "Name " ]
            , th [] [ text "Private?" ]
            , th [] [ text "Created On " ]
            , th [ class "text-center" ] [ text "Stars " ]
            , th [ class "text-center" ] [ text "Open Issues" ]
            ]
        ]


reposTableBody : Repo -> Html Msg
reposTableBody { name, private, html_url, created_at, stargazers_count, open_issues_count } =
    tr []
        [ td [] [ a [ href html_url ] [ text name ] ]
        , td [] [ text (boolToString private) ]
        , td [] [ text created_at ]
        , td [ class "text-center" ] [ text (toString stargazers_count) ]
        , td [ class "text-center" ] [ text (toString open_issues_count) ]
        ]


appendTableHeader : a -> a -> List a
appendTableHeader x y =
    x :: [ y ]


boolToString : Bool -> String
boolToString boolean =
    case boolean of
        True ->
            "True"

        False ->
            "False"



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
