module Main exposing (..)

import Browser
import Html exposing (Html, Attribute, text, div, h1, h2, p, ul, li, input, label, button)
import Html.Attributes exposing (src, class, value, type_, name, checked, classList)
import Html.Events exposing (onInput, onClick, keyCode, on, targetValue)
import Debug exposing (log)
import Json.Decode as Json
import Task
import Random
import Random.Char
import Random.String
import List.Extra

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

type Stage = Todo | Doing | InReview | Done | UndefinedStage

type Error = No | TaskNotFound String

type alias Task =
  { name: String
  , stage: Stage
  , id: String
  }

type alias TaskToBe =
  { name: String
  , stage: Stage
  }

type alias Modal =
  { open: Bool }

type alias Model =
  { tasks: List Task
  , taskToBe: TaskToBe
  , modal: Modal
  , editing: Maybe Task
  , error: Error
  }


init : ( Model, Cmd Msg )
init =
  (
    { tasks = []
    , taskToBe = { name = "", stage = Todo }
    , modal = Modal False
    , editing = Nothing
    , error = No
    }
  , Cmd.none)



---- UPDATE ----

type Msg
  = NoOp
  | SetName String
  | SetStage Stage
  | ToggleModal
  | Save String
  | Create
  | StartEditing String
  | Delete Task
  | Edit (Maybe Task) Stage

-- Update Helpers
setName : String -> TaskToBe -> TaskToBe
setName name task = 
  { task | name = name }

setStage : Stage -> TaskToBe -> TaskToBe
setStage stage task = 
  { task | stage = stage }

toggleModal : Modal -> Modal
toggleModal modal = 
  { modal | open = not modal.open }

saveNewTask : Cmd Msg
saveNewTask = 
  Random.generate Save (Random.String.string 30 Random.Char.latin)

updateTaskStage : (Maybe Task) -> Stage -> Task -> Task
updateTaskStage maybeTask stage task = 
  case maybeTask of
    Just {id} -> 
      if id == task.id then
        { task | stage = stage }
      else
        task
    Nothing ->
      task

-- REDUCER
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
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
    Create ->
      (model, saveNewTask)
    Save newId ->
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
               model.tasks ++ [{ name = model.taskToBe.name, stage = model.taskToBe.stage, id = newId }]
         }
        , Cmd.none
      )
    StartEditing id ->
      let maybeTask = List.Extra.find (\task -> task.id == id) model.tasks
      in case maybeTask of
        Just task -> 
          ({ model | editing = Just task, modal = Modal True }, Cmd.none)
        Nothing ->
          ({ model | error = TaskNotFound "You tried to update a task that doesn't exists" }, Cmd.none)
    Edit maybeTask stage ->
      ({ model
          | editing = Nothing
          , modal = Modal False
          , tasks = List.map (updateTaskStage maybeTask stage) model.tasks
        }
      , Cmd.none
      )
    Delete task -> 
      ( { model
          | tasks = model.tasks |> List.filter (\t -> t.id /= task.id)
          , modal = Modal False
        }
      , Cmd.none
      )
    ToggleModal -> 
      ( { model | modal = model.modal |> toggleModal }, Cmd.none )
      

-- MAIN VIEW
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
      , onEnter Create 
      , value model.taskToBe.name
      , class "form__input"
      ] []
    , div [ class "form__stage-selector" ]
      [ viewStageCheckbox (model.taskToBe.stage == Todo) (SetStage Todo) (translateStage Todo)
      , viewStageCheckbox (model.taskToBe.stage == Doing) (SetStage Doing) (translateStage Doing)
      , viewStageCheckbox (model.taskToBe.stage == InReview) (SetStage InReview) (translateStage InReview)
      , viewStageCheckbox (model.taskToBe.stage == Done) (SetStage Done) (translateStage Done)
      , button [ class "form__btn", onClick Create ] [ text "SAVE" ]
      ]
    , viewModal model.modal model.editing model.error
    ]
  ]

-- VIEWS

viewModal : Modal -> (Maybe Task) -> Error -> Html Msg
viewModal modal maybeTask error =
  let (id, stage) = maybeTaskToIdStageTuple maybeTask
  in div [ classList [("modal", True), ("modal--open", modal.open)] ]
      [ div [ class "modal__dialog" ]
          [ div [ class "modal__title" ] [ text ("CHANGE ACTUAL STAGE") ]
          , div [ class "modal__subtitle" ] [ text ("Editing: " ++ id  ++ " in " ++ (translateStage stage)) ]
          , viewError error
          , button
              [ classList [("modal__stage", True), ("modal__stage--selected", stage == Todo)]
              , onClick (viewHandleStageClick Todo maybeTask)
              ]
              [ text <| translateStage <| Todo ]
          , button
              [ classList [("modal__stage", True), ("modal__stage--selected", stage == Doing)]
              , onClick (viewHandleStageClick Doing maybeTask)
              ]
              [ text <| translateStage <| Doing ]
          , button
              [ classList [("modal__stage", True), ("modal__stage--selected", stage == InReview)]
              , onClick (viewHandleStageClick InReview maybeTask)
              ]
              [ text <| translateStage <| InReview ]
          , button
              [ classList [("modal__stage", True), ("modal__stage--selected", stage == Done)]
              , onClick (viewHandleStageClick Done maybeTask)
              ]
              [ text <| translateStage <| Done ]
          , button [ class "modal__stage modal__stage--danger", onClick (viewHandleTaskDelete maybeTask) ] [ text "REMOVE TASK" ]
          , button [ class "modal__close", onClick ToggleModal ] [ text "×" ]
          ]
       ]

viewHandleStageClick : Stage -> Maybe Task -> Msg
viewHandleStageClick newStage maybeTask =
 case maybeTask of
   Just task ->
     if (newStage == task.stage) then
        NoOp
     else 
        Edit maybeTask newStage
   Nothing ->
     NoOp

viewHandleTaskDelete : Maybe Task -> Msg
viewHandleTaskDelete maybeTask =
 case maybeTask of
   Just task ->
     Delete task
   Nothing ->
     NoOp

maybeTaskToIdStageTuple : Maybe Task -> (String, Stage)
maybeTaskToIdStageTuple maybeTask =
   case maybeTask of
     Just task ->
       (task.id, task.stage)
     Nothing ->
       ("NOTHING", UndefinedStage)

viewBoardColumn : List Task -> Stage -> Html Msg
viewBoardColumn tasks stage =
   div [ class "board__column" ]
    [ h2 [ class "board__column-title" ] [ text (translateStage stage) ]
    , tasks
        |> List.filter (taskIs stage)
        |> List.map viewTodo
        |> div []
    ]

viewError : Error -> Html Msg
viewError error =
  case error of 
    TaskNotFound msg ->
      div [ class "alert alert--danger" ] [ text msg ]
    No ->
      div [] []

viewStageCheckbox : Bool -> Msg -> String -> Html Msg
viewStageCheckbox isChecked msg desc =
  label [ class "form__stage" ]
    [ input
      [ type_ "radio"
      , name "stage" 
      , checked isChecked
      , onClick msg
      , onEnter Create
      , class "form__stage-checkbox" 
    ] []
    , text desc
  ]

viewBox : List (Html Msg) -> Html Msg
viewBox elements
  = div [ class "box" ] elements

viewTodo : Task -> Html Msg
viewTodo task
  = div [ class "board__task", onClick (StartEditing task.id) ] [text task.name]

-- HELPERS

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
    UndefinedStage -> "" 
