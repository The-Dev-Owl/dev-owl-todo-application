module App

open Elmish
open Elmish.React
open Feliz
open Zanaptak.TypedCssClasses

type Bulma = CssClasses<"https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.4/css/bulma.min.css", Naming.PascalCase>
type FA = CssClasses<"https://use.fontawesome.com/releases/v5.8.1/css/all.css", Naming.PascalCase>

type TodoId = TodoId of System.Guid 

type Todo =
    {
        Id : TodoId
        Description : string 
        Completed : bool
    }

type State =
    { 
        Todos : Todo list
        NewTodoDescription : string
    }

type Msg =
    | NewTodoChanged of string
    | AddNewTodo
    | DeleteTodo of TodoId

let init() =
    { 
        Todos = []
        NewTodoDescription = ""
    }


let withNewTodo description state =
    let todo =
        {
            Id = TodoId (System.Guid.NewGuid())
            Description = description
            Completed = false
        }

    { state with 
        Todos = List.append state.Todos [todo] 
        NewTodoDescription = "" 
    } 

let withoutTodo todoId state =
    let todos =
        state.Todos 
        |> List.filter (fun todo -> todo.Id <> todoId)

    { state with Todos = todos }    

let update (msg: Msg) (state: State): State =
    match msg with 
    | NewTodoChanged description ->
        { state with NewTodoDescription = description }
    
    | AddNewTodo ->
        state 
        |> withNewTodo state.NewTodoDescription

    | DeleteTodo todo ->
        state 
        |> withoutTodo todo    


let title = 
    Html.p [ 
        prop.className "title"
        prop.text "Dev-Owl Elmish To-Do App"
    ]

let newTodoInput (currentNewTodo: string) (dispatch: Msg -> unit) = 
    Html.div [ 
        prop.classes [ Bulma.Field; Bulma.HasAddons ]
        prop.children [
            Html.div [
                prop.classes [ "control"; "is-expanded" ]
                prop.children [ 
                    Html.input [
                        prop.classes [ "input"; "is-medium" ]
                        prop.onKeyUp (fun ev -> if ev.keyCode = 13.0 then AddNewTodo |> dispatch)
                        prop.valueOrDefault currentNewTodo
                        prop.onTextChange (NewTodoChanged >> dispatch)
                    ]
                ]
            ]

            Html.div [
                prop.classes [ "control" ]
                prop.children [ 
                    Html.button [
                        prop.classes [ "button"; "is-primary"; "is-medium" ]
                        prop.onClick (fun _ -> AddNewTodo |> dispatch)
                        prop.children [ 
                            Html.i [ prop.classes [ FA.Fa; FA.FaPlus ] ]
                        ]
                    ]
                ]
            ] 
        ]
    ]

// Helper function to easily construct div with only classes and children
let div (classes: string list) (children: Fable.React.ReactElement list) =
    Html.div [
        prop.classes classes
        prop.children children
    ]

let renderTodo (todo: Todo) dispatch = 
    div [ "box" ] [
        div [ "columns"; "is-mobile" ] [
            div [ "column" ] [ 
                Html.p [
                    prop.className "subtitle"
                    prop.text todo.Description
                ]
            ]

            div [ "column"; "is-narrow" ] [ 
                Html.button [ 
                    prop.classes ["button"; "is-danger"]
                    prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
                    prop.children [ 
                        Html.i [ prop.classes [ "fa"; "fa-times" ] ]
                    ]
                ]
            ]
        ]
    ]

let todoList (todos: Todo list) dispatch = 
    Html.ul [
        prop.children [ for todo in todos -> renderTodo todo dispatch ]
    ]
 
let render (state: State) (dispatch: Msg -> unit) =
    Html.div [
        prop.style [ style.padding 20 ]
        prop.children [ 
            title 
            newTodoInput state.NewTodoDescription dispatch 
            todoList state.Todos dispatch
        ]
    ]

Program.mkSimple init update render
|> Program.withReactSynchronous "elmish-app"
|> Program.run