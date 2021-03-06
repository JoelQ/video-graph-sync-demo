module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src, width, controls)
import Html.Events
import Http
import Plot
import Json.Decode as JD exposing (Decoder)


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
    { position : Float, streams : List Stream }


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
    Http.get "eda.json" (streamDecoder "Eda")
        |> Http.send ReceivedStream


fetchStream2 : Cmd Msg
fetchStream2 =
    Http.get "stream2.json" (streamDecoder "Stream 2")
        |> Http.send ReceivedStream


fetchStream3 : Cmd Msg
fetchStream3 =
    Http.get "stream3.json" (streamDecoder "Stream 3")
        |> Http.send ReceivedStream


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.batch [ fetchEda, fetchStream2, fetchStream3 ] )



-- UPDATE


type Msg
    = NoOp
    | ReceivedStream (Result Http.Error Stream)
    | TimeUpdated Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ReceivedStream (Err message) ->
            ( model, Cmd.none )

        ReceivedStream (Ok stream) ->
            ( { model | streams = stream :: model.streams }, Cmd.none )

        TimeUpdated time ->
            ( { model | position = toFloat (round time) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


onTimeUpdate : (Float -> msg) -> Html.Attribute msg
onTimeUpdate msg =
    Html.Events.on "timeupdate" (JD.map msg targetCurrentTime)


targetCurrentTime : Decoder Float
targetCurrentTime =
    JD.at [ "target", "currentTime" ] JD.float


view : Model -> Html Msg
view model =
    main_ []
        [ h1 [] [ text "Video Graph Sync Demo" ]
        , video
            [ src "https://github.com/JoelQ/video-graph-sync-demo/blob/master/video.mp4?raw=true"
            , width 500
            , controls True
            , onTimeUpdate TimeUpdated
            ]
            []
        , section [] (List.map (viewStream model.position) model.streams)
        ]


viewStream : Float -> Stream -> Html a
viewStream scrubber stream =
    div []
        [ h2 [] [ text stream.name ]
        , dataPlot scrubber stream.points
        ]


buildPoint : Float -> Point -> Plot.DataPoint msg
buildPoint scrubber point =
    let
        basicPoint =
            Plot.circle point.x point.y
    in
        if point.x == scrubber then
            { basicPoint | xLine = Just Plot.simpleLine }
        else
            basicPoint


defaultLine : Float -> Plot.Series (List Point) msg
defaultLine scrubber =
    Plot.line (List.map (buildPoint scrubber))


config : Plot.PlotCustomizations msg
config =
    let
        default =
            Plot.defaultSeriesPlotCustomizations
    in
        { default | height = 100 }


dataPlot : Float -> List Point -> Html a
dataPlot scrubber points =
    Plot.viewSeriesCustom config [ defaultLine scrubber ] points
