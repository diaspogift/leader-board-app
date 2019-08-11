module Main exposing (Model, Msg(..), Page(..), main, subscriptions, update, view, viewFooter, viewHeader)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)
import LeaderBoard
import Login
import Runner
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)



----------------------------------------------------------------------------- MODEL ------------------------------------------------------------


type alias Model =
    { page : Page
    , key : Nav.Key
    }


type Page
    = HomePage LeaderBoard.Model
    | LeaderBoardPage LeaderBoard.Model
    | LoginPage Login.Model
    | RunnerPage Runner.Model 
    | NotFound


type Route
    = Home
    | LeaderBoard
    | Runner
    | Login



--------------------------------------------------------------------------- UPDATE ------------------------------------------------------------


type Msg
    = ChangeUrl Url
    | ClickedLink Browser.UrlRequest
    | GotLeaderBoardMsg LeaderBoard.Msg
    | GotLoginMsg Login.Msg
    | GotRunnerMsg Runner.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangeUrl url ->
            let 
                _ = Debug.log "Updated Url = " url
            in
            updateUrl url model

        GotLeaderBoardMsg lbMsg ->
            case model.page of
                LeaderBoardPage leader ->
                    toLeaderBoard model (LeaderBoard.update lbMsg leader)

                _ ->
                    ( model, Cmd.none )

        GotLoginMsg lgMsg ->
            case model.page of
                LoginPage login ->
                    toLogin model (Login.update lgMsg login)

                _ ->
                    ( model, Cmd.none )


        GotRunnerMsg ruMsg ->
            case model.page of
                RunnerPage runner ->
                    toRunner model (Runner.update ruMsg runner)

                _ ->
                    ( model, Cmd.none )






toLeaderBoard : Model -> ( LeaderBoard.Model, Cmd LeaderBoard.Msg ) -> ( Model, Cmd Msg )
toLeaderBoard model ( leaderBoard, cmd ) =
    ( { model | page = LeaderBoardPage leaderBoard }
    , Cmd.map GotLeaderBoardMsg cmd
    )


toLogin : Model -> ( Login.Model, Cmd Login.Msg ) -> ( Model, Cmd Msg )
toLogin model ( login, cmd ) =
    ( { model | page = LoginPage login }
    , Cmd.map GotLoginMsg cmd
    )



toRunner : Model -> ( Runner.Model, Cmd Runner.Msg ) -> ( Model, Cmd Msg )
toRunner model ( runner, cmd ) =
    ( { model | page = RunnerPage runner }
    , Cmd.map GotRunnerMsg cmd
    )

--------------------------------------------------------------------------- VIEW -------------------------------------------------------------


view : Model -> Document Msg
view model =
    let
        content =
            case model.page of
                HomePage home ->
                    LeaderBoard.view home
                        |> Html.map GotLeaderBoardMsg

                LeaderBoardPage leaderboard ->
                    LeaderBoard.view leaderboard
                        |> Html.map GotLeaderBoardMsg

                LoginPage login ->
                    Login.view login
                        |> Html.map GotLoginMsg

                RunnerPage runner ->
                    Runner.view runner
                        |> Html.map GotRunnerMsg

                NotFound ->
                    text "Not Found!"

    in
    { title = "Leader Board App"
    , body =
        [ lazy viewHeader model.page
        , content
        , viewFooter
        ]
    }


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Leader Board Live" ]

        links =
            ul
                []
                [ navLink LeaderBoard { url = "/leaderboard", caption = "LeaderBoard" }
                , navLink LeaderBoard { url = "/addrunner", caption = "AddRunner" }
                , navLink Login { url = "/login", caption = "Login" }
                ]

        navLink : Route -> { url : String, caption : String } -> Html msg
        navLink route { url, caption } =
            li [ classList [ ( "active", isActive { link = route, page = page } ) ] ]
                [ a [ href url ] [ text caption ] ]
    in
    nav [] [ logo, links ]


isActive : { link : Route, page : Page } -> Bool
isActive { link, page } =

    let 
            _ = Debug.log "In isActive link = " link
            _ = Debug.log "In isActive page = " page

    in
    case ( link, page ) of
        ( LeaderBoard, LeaderBoardPage _ ) ->
            True

        ( LeaderBoard, _ ) ->
            False

        ( Login, LoginPage _ ) ->
            True

        ( Login, _ ) ->
            False

        ( _, _ ) ->
            False


viewFooter : Html msg
viewFooter =
    footer [] [ text "One is never alone with a rubber duck. -Douglas Adams" ]



------------------------------------------------------------------------------ MAIN -----------------------------------------------------------


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangeUrl
        }


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    updateUrl url { page = NotFound, key = key }


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse parser url of
        Just Home ->
            toLeaderBoard model LeaderBoard.init

        Just LeaderBoard ->
            toLeaderBoard model LeaderBoard.init

        Just Login ->
            toLogin model Login.init

        Just Runner ->
            toRunner model Runner.init

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map LeaderBoard (s "leaderboard")
        , Parser.map Login (s "login")
        , Parser.map Runner (s "addrunner")
        ]
