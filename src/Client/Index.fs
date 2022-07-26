module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Model = { Todos: Todo list; Input: string }

type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model = { Todos = []; Input = "" }

    let cmd = Cmd.OfAsync.perform todosApi.getTodos () GotTodos

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos }, Cmd.none
    | SetInput value -> { model with Input = value }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.Input

        let cmd = Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

        { model with Input = "" }, cmd
    | AddedTodo todo -> { model with Todos = model.Todos @ [ todo ] }, Cmd.none

open Feliz
open Feliz.Bulma

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            navbarItem.isActive
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                ]
            ]
        ]
    ]

module rec Cytoscape =

    open Fable
    open Fable.Core
    open Fable.Core.JS

    [<ImportDefault("Cytoscape")>]
    let cy(options:obj): ICytoscape = jsNative

    type ILayout =
        abstract member run: unit -> unit

    type ICytoscape =
        abstract member mount: Browser.Types.HTMLElement -> unit
        abstract member unmount: unit -> unit
        abstract member add: obj -> unit
        abstract member center: unit -> unit
        abstract member fit: unit -> unit
        abstract member layout: options:obj -> ILayout
        abstract member bind: event:string -> element:string -> (Browser.Types.MouseEvent -> unit) -> unit


let mutable cy : Cytoscape.ICytoscape option = None

let createNode id =
    {|data = {|id = id|}|} |> box

let createEdge id source target =
    {|data = {|id = id; source = source; target = target|}|} |> box

open Fable.Core.JsInterop

let cytoscapeBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        prop.children [
            Html.div [prop.id "cy"]
            Bulma.field.div [
                field.isGrouped
                prop.children [
                    Bulma.control.p [
                        Bulma.button.a [
                            color.isPrimary
                            prop.onClick (fun _ ->
                                let element = Browser.Dom.document.getElementById "cy"
                                let cy_ele =
                                    Cytoscape.cy({|
                                        container = element;
                                        elements = seq [
                                            createNode "a"
                                            createNode "b"
                                            createNode "c"
                                            createEdge "ab" "a" "b"
                                            createEdge "ac" "a" "c"
                                        ];
                                        style = seq [
                                            {|
                                                selector = "node"
                                                style = {|
                                                    label = "data(id)"
                                                |} |> box
                                            |};
                                            {|
                                                selector = "edge"
                                                style = createObj [
                                                    "width" ==> 3
                                                    "line-color" ==> "blue"
                                                ]
                                            |}
                                        ]
                                    |})
                                cy_ele.bind "click" "node" (fun e -> Browser.Dom.console.log(e.target?id()))
                                cy_ele.center()
                                cy <- Some cy_ele

                            )
                            prop.text "Add Cytoscape"
                        ]
                        Bulma.button.a [
                            color.isPrimary
                            prop.onClick (fun _ ->
                                cy.Value.add <| createNode "d"
                                cy.Value.add <| createEdge "da" "d" "a"
                                cy.Value.add <| createEdge "db" "d" "b"
                                cy.Value.add <| createEdge "dc" "d" "c"
                                let layout = cy.Value.layout({|name = "breadthfirst"|})
                                layout.run()

                            )
                            prop.text "Add nodes and layout update"
                        ]
                        //Bulma.button.a [
                        //    color.isPrimary
                        //    prop.onClick (fun _ ->
                        //        Browser.Dom.console.log (cy)

                        //    )
                        //    prop.text "Check Cytoscape"
                        //]
                    ]
                ]
            ]
        ]
    ]

let containerBox (model: Model) (dispatch: Msg -> unit) =
    Bulma.box [
        Bulma.content [
            Html.ol [
                for todo in model.Todos do
                    Html.li [ prop.text todo.Description ]
            ]
        ]
        Bulma.field.div [
            field.isGrouped
            prop.children [
                Bulma.control.p [
                    control.isExpanded
                    prop.children [
                        Bulma.input.text [
                            prop.value model.Input
                            prop.placeholder "What needs to be done?"
                            prop.onChange (fun x -> SetInput x |> dispatch)
                        ]
                    ]
                ]
                Bulma.control.p [
                    Bulma.button.a [
                        color.isPrimary
                        prop.disabled (Todo.isValid model.Input |> not)
                        prop.onClick (fun _ -> dispatch AddTodo)
                        prop.text "Add"
                    ]
                ]
            ]
        ]
    ]

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ navBrand ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is6
                        column.isOffset3
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "CytoscapeTest"
                            ]
                            cytoscapeBox model dispatch
                            containerBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]