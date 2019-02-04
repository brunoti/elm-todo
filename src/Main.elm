module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, text, div, h1, p, ul, li, input, label)
import Html.Attributes exposing (src, class, value, type_, name, checked)
import Html.Events exposing (onInput, onClick, keyCode, on, targetValue)
import Debug exposing (log)
import Json.Decode as Json



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
  box 
  [ h1 [ class "box__title" ] [ text "The Final Elm Todo App" ]
  , renderTodoList model.tasks
  , input [ onInput SetName, onEnter Save, value model.taskToBe.name ] []
  , label []
      [ input
        [ type_ "radio"
        , name "stage" 
        , checked (model.taskToBe.stage == Todo)
        , onClick (SetStage Todo)
        , onEnter Save
        ] []
        , text (translateStage Todo)
      ]
  , label []
      [ input
        [ type_ "radio"
        , name "stage" 
        , checked (model.taskToBe.stage == Doing)
        , onClick (SetStage Doing)
        , onEnter Save
        ] []
        , text (translateStage Doing)
      ]
  , label []
      [ input
        [ type_ "radio"
        , name "stage" 
        , checked (model.taskToBe.stage == InReview)
        , onClick (SetStage InReview)
        ] []
        , text (translateStage InReview)
      ]
  , label []
      [ input
        [ type_ "radio"
        , name "stage" 
        , checked (model.taskToBe.stage == Done)
        , onClick (SetStage Done)
        , onEnter Save
        ] []
        , text (translateStage Done)
      ]
  ]

translateStage : Stage -> String
translateStage stage =
  case stage of
    Todo -> "Todo" 
    Doing -> "Doing" 
    InReview -> "In Review" 
    Done -> "Done" 


box : List (Html Msg) -> Html Msg
box elements
  = div [ class "box" ] elements

renderTodoList : List Task -> Html Msg
renderTodoList tasks
  = ul []
  (List.map renderTodo tasks)

renderTodo : Task -> Html Msg
renderTodo task
  = li [] [text (task.name ++ " " ++ "[" ++ (translateStage task.stage) ++ "]")]

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

---- PROGRAM ----


main : Program () Model Msg
main =
  Browser.element
  { view = view
  , init = \_ -> init
  , update = update
  , subscriptions = always Sub.none
  }
