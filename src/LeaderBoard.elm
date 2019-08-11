module LeaderBoard exposing (Model, Msg, update, view, initModel, init)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



----------------------------------------------------------------------------- MODEL ------------------------------------------------------------


type alias Model =
    { runners : List Runner
    , query : String
    , active : Bool
    , error : Maybe String
    }


type alias Runner =
    { id : String
    , name : String
    , location : String
    , age : Int
    , bib : Int
    , estimatedDistance : Float
    , lastMarkerDistance : Float
    , lastMarkerTime : Float
    , pace : Float
    }



tempRunners : List Runner
tempRunners =
    [ Runner "1" "James Moore" "Turlock CA" 42 1234 0 1 1463154945381 0.125
    , Runner "2" "Meb Keflezighi" "Turlock CA" 41 1238 0 1 1463154945381 0.09
    ]


initModel : Model
initModel =
    { error = Nothing
    , query = ""
    , runners = tempRunners
    , active = False
    }



init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


--------------------------------------------------------------------------- UPDATE ------------------------------------------------------------


type Msg
    = SearchInput String
    | Search




update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput query ->
            ( { model | query = query }, Cmd.none )
        
        Search ->
            ( model, Cmd.none )


--------------------------------------------------------------------------- VIEW -------------------------------------------------------------


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ errorPanel model.error
        , searchForm model.query
        , viewRunners model
        ]


errorPanel : Maybe String -> Html a
errorPanel error =
    case error of
        Nothing ->
            text ""

        Just msg ->
            div [ class "error" ]
                [ text msg
                , button [ type_ "button" ] [ text "×" ]
                ]


searchForm : String -> Html Msg
searchForm query =
    Html.form [ onSubmit Search ]
        [ input
            [ type_ "text"
            , placeholder "Search for runner..."
            , value query
            , onInput SearchInput
            ]
            []
        , button [ type_ "submit" ] [ text "Search" ]
        ]


viewRunners : Model -> Html Msg
viewRunners { query, runners, active, error } =
    runners
        |> List.map runner
        |> tbody []
        |> (\r -> runnersHeader :: [ r ])
        |> table []


runner : Runner -> Html Msg
runner { name, location, age, bib, estimatedDistance } =
    tr []
        [ td [] [ text name ]
        , td [] [ text location ]
        , td [] [ text (String.fromInt age) ]
        , td [] [ text (String.fromInt bib) ]
        , td []
            [ text "1 mi @ 08:30AM (TODO)"
            ]
        , td [] [ text (String.fromFloat estimatedDistance) ]
        ]


runnersHeader : Html Msg
runnersHeader =
    thead []
        [ tr []
            [ th [] [ text "Name" ]
            , th [] [ text "From" ]
            , th [] [ text "Age" ]
            , th [] [ text "Bib #" ]
            , th [] [ text "Last Marker" ]
            , th [] [ text "Est. Miles" ]
            ]
        ]
