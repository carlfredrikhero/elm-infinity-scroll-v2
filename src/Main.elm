port module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events exposing (..)
import Html.Keyed
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import Task exposing (Task)


type Sentinel
    = Top
    | Bottom


stringToSentinel : String -> Maybe Sentinel
stringToSentinel string =
    case string of
        "sentinel-top" ->
            Just Top

        "sentinel-bottom" ->
            Just Bottom

        _ ->
            Nothing


port loadmore : (String -> msg) -> Sub msg


style : Model -> String
style { total, start, end, itemsPerRow } =
    let
        totalRows =
            ceiling (toFloat total / toFloat itemsPerRow)

        itemRows =
            ceiling (toFloat (end - start) / toFloat itemsPerRow)

        remainingRows =
            totalRows - itemRows

        padding =
            remainingRows * 66

        paddingString =
            String.fromInt padding ++ "px;"
    in
    """
.grid {
    /*display: grid;
    grid-gap: 1rem;
    grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));*/
    display: flex;
    flex-wrap: wrap;
    margin: -0.5rem;
    padding-bottom: """ ++ paddingString ++ """
}

.grid > div {
    background: lightgreen;
    min-height: 50px;
    flex: 1 1 150px;
    margin: 0.5rem;
    max-width: 183px;
}

.grid > .sentinel {
  border: 1px solid red;
  flex: 0 0 100%;
  height: 1px;
  min-height: 1px;
  background: pink;
  margin: 0;
  max-width: unset;
}
"""



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { start : Int
    , end : Int
    , limit : Int
    , total : Int
    , items : Dict Int (Status Item)
    , itemsPerRow : Int
    }


type Status a
    = NotLoaded
    | Loading
    | Loaded a
    | Failed String


type alias Item =
    { id : Int
    , title : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        start =
            0

        limit =
            24

        end =
            (limit + start) - 1
    in
    ( { start = start
      , limit = limit
      , end = end
      , total = 1000
      , items =
            Dict.fromList
                (List.range start end
                    |> List.map (\i -> Tuple.pair i Loading)
                )
      , itemsPerRow = 1
      }
    , Cmd.batch
        [ calcHeight 0
        , fetch start end
        ]
    )


calcHeight : Int -> Cmd Msg
calcHeight index =
    Browser.Dom.getElement (String.fromInt index)
        |> Task.attempt (GotElement index)


fetch : Int -> Int -> Cmd Msg
fetch start end =
    Http.get
        { url = "http://localhost:3000/products?_start=" ++ String.fromInt start ++ "&_end=" ++ String.fromInt (end + 1)
        , expect = Http.expectJson (GotItems ( start, end )) (Decode.list decodeItem)
        }


decodeItem : Decoder Item
decodeItem =
    Decode.map2 Item (Decode.field "id" Decode.int) (Decode.field "title" string)



-- UPDATE


type Msg
    = NoOp
    | GotElement Int (Result Browser.Dom.Error Browser.Dom.Element)
    | WindowResized
    | GotItems ( Int, Int ) (Result Http.Error (List Item))
    | LoadMore String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotElement index (Ok el) ->
            let
                ( itemsPerRow, finished ) =
                    if el.element.y > 50 then
                        ( index, True )

                    else
                        ( 1, False )

                cmd =
                    if finished then
                        Cmd.none

                    else
                        calcHeight (index + 1)
            in
            ( { model | itemsPerRow = itemsPerRow }, cmd )

        GotElement _ (Err error) ->
            let
                _ =
                    Debug.log "got element err" error
            in
            ( model, Cmd.none )

        WindowResized ->
            ( model, calcHeight 0 )

        GotItems ( min, max ) (Ok items) ->
            let
                updatedItems =
                    items
                        |> List.map2 (\rowNo item -> ( rowNo, Loaded item )) (List.range min max)
                        |> Dict.fromList
            in
            ( { model | items = Dict.union updatedItems model.items }, Cmd.none )

        GotItems _ _ ->
            let
                _ =
                    Debug.log "Got Items error"
            in
            ( model, Cmd.none )

        LoadMore string ->
            let
                _ =
                    Debug.log "maybeSentinel" (stringToSentinel string)
            in
            updateLoad (stringToSentinel string) model


updateLoad : Maybe Sentinel -> Model -> ( Model, Cmd Msg )
updateLoad maybeSentinel model =
    maybeSentinel
        |> Maybe.andThen (fetchRange model)
        |> Maybe.map
            (\( start, end ) ->
                let
                    loadingItems =
                        List.range start end
                            |> List.map (\i -> Tuple.pair i Loading)
                            |> Dict.fromList
                in
                ( { model
                    | items = Dict.union loadingItems model.items
                    , start = min model.start start
                    , end = max model.end end
                  }
                , fetch start end
                )
            )
        |> Maybe.withDefault ( model, Cmd.none )


fetchRange : Model -> Sentinel -> Maybe ( Int, Int )
fetchRange { start, limit, end, total } sentinel =
    case sentinel of
        Top ->
            if start == 0 then
                Nothing

            else
                let
                    nextStart =
                        max 0 (start - limit)

                    nextEnd =
                        min start (nextStart + limit)
                in
                Just ( nextStart, nextEnd )

        Bottom ->
            if end == total then
                Nothing

            else
                let
                    nextStart =
                        end + 1

                    nextEnd =
                        min total ((nextStart + limit) - 1)
                in
                Just ( nextStart, nextEnd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize (\w h -> WindowResized)
        , loadmore LoadMore
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Hello World"
    , body =
        [ div []
            [ Html.node "style" [] [ text (style model) ]
            , div [] [ Html.text ("Items per row: " ++ String.fromInt model.itemsPerRow) ]
            , div [ Attributes.classList [ ( "grid", True ) ] ]
                (viewSentinel "top" :: List.map viewItem (Dict.toList model.items) ++ [ viewSentinel "bottom" ])
            ]
        ]
    }


viewSentinel : String -> Html Msg
viewSentinel suffix =
    let
        s =
            "sentinel"
    in
    Html.Keyed.node "div" [ Attributes.classList [ ( s, True ) ], Attributes.id (String.join "-" [ s, suffix ]) ] [ ( suffix, Html.text "" ) ]


viewItem : ( Int, Status Item ) -> Html Msg
viewItem ( index, statusItem ) =
    let
        content =
            case statusItem of
                NotLoaded ->
                    "Not Loaded"

                Loading ->
                    "Loading..."

                Loaded item ->
                    String.fromInt item.id ++ ": " ++ item.title

                Failed err ->
                    "Failed: " ++ err
    in
    Html.Keyed.node "div" [ Attributes.id (String.fromInt index) ] [ ( String.fromInt index, Html.text content ) ]
