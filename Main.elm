module Main exposing (..)

import Time exposing (Time, second)
import Keyboard.Extra
import Html exposing (Html)
import Tuple.Extra as Tuple exposing (..)
import Elegant


initialKeyboard : Keyboard.Extra.Model
initialKeyboard =
    Tuple.first Keyboard.Extra.init


type alias Vector =
    ( Float, Float )


type alias Element =
    { location : Vector
    , velocity : Vector
    , acceleration : Vector
    , mass : Float
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
    }


type Msg
    = Tick Time
    | KeyboardMsg Keyboard.Extra.Msg


init : ( Model, Cmd Msg )
init =
    ( { config =
            { screenSize = ( 640, 480 )
            , backgroundColor = "#000"
            , keyboardModel = initialKeyboard
            , arrows = ( 0, 0 )
            , fps = 60.0
            }
      , elements =
            [ { location = ( 300, 200 )
              , velocity = ( 0, 0 )
              , acceleration = ( 0, 0 )
              , mass = 1
              , sprite = "⚽"
              }
            , { location = ( 200, 100 )
              , velocity = ( 0, 0 )
              , acceleration = ( 0, 0 )
              , mass = 0.4
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
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardMsg Keyboard.Extra.subscriptions
        , Time.every (second / model.config.fps) Tick
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


handleKeyboard : Model -> Keyboard.Extra.Msg -> ( Model, Cmd Msg )
handleKeyboard model keyMsg =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.Extra.update keyMsg model.config.keyboardModel

        arrows =
            Keyboard.Extra.arrows keyboardModel

        config =
            model.config

        newConfig =
            { config
                | keyboardModel = keyboardModel
                , arrows = ( arrows.x |> toFloat, arrows.y |> toFloat )
            }
    in
        ( { model | config = newConfig }
        , Cmd.map KeyboardMsg keyboardCmd
        )


applyVelocity ({ location, velocity } as element) =
    { element
        | location = Tuple.add location velocity
    }


applyAcceleration ({ velocity, acceleration } as element) =
    { element
        | velocity = Tuple.add velocity acceleration
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


applyWorldLimits ( limitX, limitY ) ({ velocity, location } as element) =
    case location of
        ( locX, locY ) ->
            { element
                | velocity =
                    velocity
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


wind : Vector
wind =
    ( 0.01, 0 )


gravity : Vector
gravity =
    ( 0, 0.1 )


friction : Vector -> Vector
friction =
    mult -1
        >> normalize
        >> mult 0.01


applyForces ({ mass, velocity } as element) =
    let
        addForce force =
            add (div mass force)
    in
        { element
            | acceleration =
                (( 0, 0 )
                    |> addForce wind
                    |> add gravity
                    |> addForce (friction velocity)
                )
        }


tickElement : Vector -> Element -> Element
tickElement screenSize =
    applyVelocity
        >> applyWorldLimits screenSize
        >> applyAcceleration
        >> applyForces


tickElements ({ elements } as model) =
    { model
        | elements =
            elements |> List.map (tickElement model.config.screenSize)
    }


tick =
    tickElements


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyboardMsg keyMsg ->
            handleKeyboard model keyMsg

        Tick time ->
            ( model
                |> tick
            , Cmd.none
            )
