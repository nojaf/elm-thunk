module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode

type alias Model =
    { userName : String
    , repos : List String
    , errorMessage : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( (Model "" [] Nothing), Cmd.none )


type Msg
    = ChangeUserName String
    | LoadRepositories
    | RepositoriesResponse (Result Http.Error (List String))


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "columns" ]
            [ div [ class "is-one-third" ]
                [ createForm model
                ]
            ]
        , hr [] []
        , div [ class "columns" ]
            [ div [ class "is-half" ]
                [ createTable model.repos
                ]
            ]
        ]


createForm : Model -> Html Msg
createForm model =
    Html.form [ onSubmit LoadRepositories ]
        [ label [ class "label" ] [ text "github user" ]
        , p [ class "control" ]
            [ input [ class "input", type_ "text", value model.userName, onInput ChangeUserName ] []
            ]
        , p [ class "control" ]
            [ input [ type_ "submit", value "Submit", class "button is-primary" ] []
            ]
        ]


createTable : List String -> Html Msg
createTable repos =
    table [ class "table" ]
        [ thead []
            [ th [] [ text "Repository" ]
            ]
        , tbody [] (List.map createRow repos)
        ]


createRow : String -> Html Msg
createRow repo =
    tr []
        [ td [] [ text repo ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeUserName name ->
            ( { model | userName = name }, Cmd.none )

        LoadRepositories ->
            let
                request =
                    Http.get (getUrl model.userName) decodeRepos

                cmd =
                    Http.send RepositoriesResponse <| request
            in
                ( model, cmd )

        RepositoriesResponse (Ok repos) ->
            ( { model | repos = repos }, Cmd.none )

        RepositoriesResponse (Err err) ->
            ( { model | errorMessage = Just (toString err) }, Cmd.none )


decodeRepos : Json.Decode.Decoder (List String)
decodeRepos =
    Json.Decode.at [ "full_name" ] Json.Decode.string
        |> Json.Decode.list


getUrl : String -> String
getUrl userName =
    "https://api.github.com/users/" ++ userName ++ "/repos"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
