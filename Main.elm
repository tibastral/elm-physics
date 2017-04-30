module Main exposing (..)

import Time exposing (Time, second)
import Keyboard.Extra
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Tuple.Extra as Tuple exposing (..)
import Elegant


initialKeyboard : Keyboard.Extra.Model
initialKeyboard =
    Tuple.first Keyboard.Extra.init


type alias Vector =
    ( Float, Float )


type alias Element =
    { location : Vector
    , v : Vector
    , a : Vector
    , m : Float
    , sprite : String
    }


type alias Config =
    { screenSize : Vector
    , backgroundColor : String
    , keyboardModel : Keyboard.Extra.Model
    , arrows : Vector
    , fps : Float
    }


type alias Model =
    { config : Config
    , elements : List Element
    , wind : String
    }


type Msg
    = Tick Time
    | UpdateWind String


init : ( Model, Cmd Msg )
init =
    ( { wind = "0"
      , config =
            { screenSize = ( 640, 480 )
            , backgroundColor = "#000"
            , keyboardModel = initialKeyboard
            , arrows = ( 0, 0 )
            , fps = 60.0
            }
      , elements =
            [ { location = ( 300, 200 )
              , v = ( 0, 0 )
              , a = ( 0, 0 )
              , m = 1
              , sprite = "⚽"
              }
            , { location = ( 200, 100 )
              , v = ( 0, 0 )
              , a = ( 0, 0 )
              , m = 10
              , sprite = "⚽"
              }
            ]
      }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


elementView : Element -> Html Msg
elementView { location, sprite } =
    Html.div
        [ Elegant.style [ Elegant.absolutelyPositionned location ] ]
        [ Html.text sprite ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            (model.elements |> List.map elementView)
        , Html.input [ Html.Attributes.type_ "text", Html.Attributes.value model.wind, Html.Events.onInput UpdateWind ] []
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (second / model.config.fps) Tick
        ]


collision : a -> List a -> Bool
collision =
    List.member


between : Vector -> Float -> Bool
between ( minimum, maximum ) value =
    value >= minimum && value <= maximum


out : Vector -> Float -> Bool
out limits value =
    not (between limits value)


applyVelocity ({ location, v } as element) =
    { element
        | location = Tuple.add location v
    }


applyAcceleration ({ v, a } as element) =
    { element
        | v = Tuple.add v a
    }


magnitude : ( Float, Float ) -> Float
magnitude ( x, y ) =
    sqrt (x * x + y * y)


mult : Float -> Vector -> Vector
mult scalar =
    Tuple.map2 (*) ( scalar, scalar )


multVec : Vector -> Vector -> Vector
multVec =
    Tuple.map2 (*)


div : Float -> Vector -> Vector
div scalar vector =
    Tuple.map2 (/) vector ( scalar, scalar )


normalize : Vector -> Vector
normalize vector =
    let
        magnitude_ =
            magnitude vector
    in
        if magnitude_ == 0 then
            vector
        else
            vector |> div magnitude_


applyWorldLimits ( limitX, limitY ) ({ v, location } as element) =
    case location of
        ( locX, locY ) ->
            { element
                | v =
                    v
                        |> multVec
                            ( if between ( 0, limitX ) locX then
                                1
                              else
                                -1
                            , if between ( 0, limitY ) locY then
                                1
                              else
                                -1
                            )
                , location =
                    ( if locX < 0 then
                        0
                      else if locX > limitX then
                        limitX
                      else
                        locX
                    , if locY < 0 then
                        0
                      else if locY > limitY then
                        limitY
                      else
                        locY
                    )
            }



-- windForce : Vector


windForce val =
    ( val, 0 )


gravity : Vector
gravity =
    ( 0, 0.05 )


friction : Vector -> Vector
friction =
    mult -1
        >> normalize
        >> mult 0.01


applyForces wind ({ m, v } as element) =
    let
        addForce force =
            add (div m force)
    in
        { element
            | a =
                (( 0, 0 )
                    |> add gravity
                    |> addForce (windForce wind)
                    |> addForce (friction v)
                )
        }


tickElement screenSize wind =
    applyVelocity
        >> applyWorldLimits screenSize
        >> applyAcceleration
        >> (applyForces wind)


tickElements : Model -> Model
tickElements ({ elements, wind } as model) =
    { model
        | elements =
            elements |> List.map (tickElement model.config.screenSize (wind |> String.toFloat |> Result.withDefault 0))
    }


tick =
    tickElements


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateWind wind ->
            ( { model | wind = wind }, Cmd.none )

        Tick time ->
            ( tick model, Cmd.none )
