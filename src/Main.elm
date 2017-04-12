module Main exposing (..)

import Html exposing (..)
import Http
import Plot
import Json.Decode as JD exposing (Decoder)


--import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Point =
    { x : Float, y : Float }


type alias Stream =
    { name : String
    , points : List Point
    }


type alias Model =
    { position : Int, streams : List Stream }


initialModel : Model
initialModel =
    { position = 0, streams = [] }


pointDecoder : Decoder Point
pointDecoder =
    JD.map2 Point (JD.field "x" JD.float) (JD.field "y" JD.float)


streamDecoder : String -> Decoder Stream
streamDecoder name =
    JD.map2 Stream (JD.succeed name) (JD.list pointDecoder)


fetchEda : Cmd Msg
fetchEda =
    Http.get "/eda.json" (streamDecoder "Eda")
        |> Http.send ReceivedEda


init : ( Model, Cmd Msg )
init =
    ( initialModel, fetchEda )



-- UPDATE


type Msg
    = NoOp
    | ReceivedEda (Result Http.Error Stream)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ReceivedEda (Err message) ->
            ( model, Cmd.none )

        ReceivedEda (Ok stream) ->
            ( { model | streams = stream :: model.streams }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    main_ []
        [ h1 [] [ text "Video Graph Sync Demo" ]
        , section [] (List.map viewStream model.streams)
        ]


viewStream : Stream -> Html a
viewStream stream =
    div []
        [ h2 [] [ text stream.name ]
        , dataPlot stream.points
        ]


defaultLine : Plot.Series (List Point) msg
defaultLine =
    Plot.line (List.map (\{ x, y } -> Plot.circle x y))


config : Plot.PlotCustomizations msg
config =
    let
        default =
            Plot.defaultSeriesPlotCustomizations
    in
        { default | height = 100 }


dataPlot : List Point -> Html a
dataPlot points =
    Plot.viewSeriesCustom config [ defaultLine ] points
