module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, text, div, h1, h2, p, ul, li, input, label, button)
import Html.Attributes exposing (src, class, value, type_, name, checked)
import Html.Events exposing (onInput, onClick, keyCode, on, targetValue)
import Debug exposing (log)
import Json.Decode as Json

---- PROGRAM ----

main : Program () Model Msg
main =
  Browser.element
  { view = view
  , init = \_ -> init
  , update = update
  , subscriptions = always Sub.none
  }


---- MODEL ----

type Stage = Todo | Doing | InReview | Done

type alias Task
  = { name: String
    , stage: Stage
    }

type alias Model =
  { tasks: List Task
  , taskToBe: Task
  }


init : ( Model, Cmd Msg )
init =
  (
    { tasks = [{ name = "Make Soda", stage = Todo }]
    , taskToBe = { name = "", stage = Todo }
    }
  , Cmd.none)



---- UPDATE ----


type Msg
  = SetName String
  | SetStage Stage
  | Save

setName : String -> Task -> Task
setName name task = 
  { task | name = name }

setStage : Stage -> Task -> Task
setStage stage task = 
  { task | stage = stage }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    SetName name ->
      (
        { model
          | taskToBe =
            model.taskToBe
            |> setName name
          }
        , Cmd.none
      )
    SetStage stage ->
      (
        { model
        | taskToBe =
          model.taskToBe
          |> setStage stage
        }
        , Cmd.none
      )
    Save ->
      (
        { model
      | taskToBe =
        model.taskToBe
        |> setStage Todo
        |> setName ""
        , tasks =
          if String.isEmpty model.taskToBe.name then
             model.tasks
             else
             model.tasks ++ [{ name = model.taskToBe.name, stage = model.taskToBe.stage }]
           }
        , Cmd.none
      )

---- VIEW ----


view : Model -> Html Msg
view model = 
  viewBox 
  [ div [ class "board" ]
      [ viewBoardColumn model.tasks Todo
      , viewBoardColumn model.tasks Doing
      , viewBoardColumn model.tasks InReview
      , viewBoardColumn model.tasks Done
      ]
  , div [ class "form" ]
    [ input
      [ onInput SetName
      , onEnter Save 
      , value model.taskToBe.name
      , class "form__input"
      ] []
    , div [ class "form__stage-selector" ]
      [ viewStageCheckbox (model.taskToBe.stage == Todo) (SetStage Todo) (translateStage Todo)
      , viewStageCheckbox (model.taskToBe.stage == Doing) (SetStage Doing) (translateStage Doing)
      , viewStageCheckbox (model.taskToBe.stage == InReview) (SetStage InReview) (translateStage InReview)
      , viewStageCheckbox (model.taskToBe.stage == Done) (SetStage Done) (translateStage Done)
      , button [ class "form__btn", onClick Save ] [ text "SAVE" ]
      ]
    ]
  ]


viewBoardColumn : List Task -> Stage -> Html Msg
viewBoardColumn tasks stage =
   div [ class "board__column" ]
    [ h2 [ class "board__column-title" ] [ text (translateStage stage) ]
    , tasks
        |> List.filter (taskIs stage)
        |> List.map viewTodo
        |> div []
    ]

taskIs : Stage -> Task -> Bool
taskIs stage task =
  stage == task.stage

translateStage : Stage -> String
translateStage stage =
  case stage of
    Todo -> "Todo" 
    Doing -> "Doing" 
    InReview -> "In Review" 
    Done -> "Done" 

viewStageCheckbox : Bool -> Msg -> String -> Html Msg
viewStageCheckbox isChecked msg desc =
  label [ class "form__stage" ]
    [ input
      [ type_ "radio"
      , name "stage" 
      , checked isChecked
      , onClick msg
      , onEnter Save
      , class "form__stage-checkbox" 
    ] []
    , text desc
  ]

viewBox : List (Html Msg) -> Html Msg
viewBox elements
  = div [ class "box" ] elements

viewTodo : Task -> Html Msg
viewTodo task
  = div [ class "board__task" ] [text task.name]

onEnter : Msg -> Attribute Msg
onEnter msg =
  let
      isEnter code =
        if code == 13 then
           Json.succeed msg
           else
           Json.fail "not ENTER"
  in
     on "keydown" (Json.andThen isEnter keyCode)

