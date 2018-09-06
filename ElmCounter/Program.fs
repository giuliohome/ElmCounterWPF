module App 

open Elmish
open Elmish.WPF
open System
open FsXaml
open System.Windows.Threading

type State = { Count: int; State: string; MyName: string } 

type GoOnMsg = {Msg: string; ToGo: int}
type GoOnAdding = {Adder: int; ToGo: int}

type Msg = 
    | Increment 
    | IncrementMofN of int * int 
    | Decrement
    | IncrementDelayed
    | IncrementTimes of int
    | ShowMsg of string
    | ShowMsgButGoOn of GoOnMsg
    | AddAndGoOn of GoOnAdding
    | StartSeqMsgs
    | SayHello
    | NameIs of string
    | ShowName

let rnd = Random()

let init() = { Count = 0; State="Ready"; MyName= "Mister X" }, Cmd.none

let simulateException level = 
    if rnd.Next(level) = level-1 
    then failwith "intentional randomic exception"

let timeout (n, msg) = async {
    do! Async.Sleep n 
    simulateException 5
    return msg 
}

let justSleep secs = async {
    do! Async.Sleep (secs/2 )
    simulateException 4
    do! Async.Sleep (secs/2 )
}

let asyncSeqEvery milliseconds (messages: Msg list) : Cmd<Msg> = 
  [ fun (dispatch: Msg -> unit) -> 
        async {
           for msg in messages do 
               do! Async.Sleep milliseconds 
               dispatch msg
        } |> Async.StartImmediate ] 
// then you can use it like this: 
// StartSeqMsgs -> state, asyncSeqEvery 1000 [Increment; Increment; Increment]

type HelloWindow = XAML<"SecondWindow.xaml">
type MainWindow = XAML<"MainWindow.xaml"> 

let mainWindow = MainWindow()

let update msg state =  
    match msg with 
    | Increment -> 
        let nextState = { state with Count = state.Count + 1; State="Incremented by 1" }
        nextState, Cmd.none 
        
    | IncrementMofN (m, n) -> 
        let nextState = { state with Count = state.Count + 1; State= sprintf "Incremented by %d of %d" m n }
        nextState, Cmd.none 

    | Decrement -> 
        let nextState = { state with Count = state.Count - 1; State="Decremented by 1" }
        nextState, Cmd.none 

    | IncrementDelayed -> 
        state, Cmd.ofAsync timeout (1000, Increment) id (fun ex -> ShowMsg ex.Message)

    | IncrementTimes n ->
        if n>0
        then 
            state, Cmd.ofAsync justSleep  2000 (fun () -> AddAndGoOn {Adder=1; ToGo=n-1}) (fun ex -> ShowMsgButGoOn {Msg=ex.Message; ToGo=n-1})
        else
            state, Cmd.ofMsg (ShowMsg "Iteration Completed")

    | ShowMsg msg -> 
        {state with State=msg }, Cmd.none

    | ShowMsgButGoOn msg -> 
        {state with State= sprintf "%s (%d to go)" msg.Msg msg.ToGo}, Cmd.ofMsg (IncrementTimes msg.ToGo)
    
    | AddAndGoOn msg ->
        {state with Count = state.Count + msg.Adder; State= sprintf "%d iterations to go" msg.ToGo }, Cmd.ofMsg (IncrementTimes msg.ToGo)

    | StartSeqMsgs -> state, asyncSeqEvery 1000 ([1..3] |> List.map (fun i -> IncrementMofN (i, 3)))

    | SayHello ->    
        mainWindow.Dispatcher.Invoke(fun () -> 
            let helloWin = HelloWindow()
            helloWin.DataContext <- mainWindow.DataContext
            helloWin.ShowDialog()) |> ignore
        state, Cmd.none
    
    | NameIs myName -> { state with MyName = myName }, Cmd.ofMsg  ShowName
    | ShowName -> {state with State= sprintf "Hello %s" state.MyName }, Cmd.none 

let bindings model dispatch = [
    
    "Count"            |> Binding.oneWay (fun state -> state.Count)
    "State"            |> Binding.oneWay (fun state -> state.State)
    "Increment"        |> Binding.cmd (fun state -> Increment)
    "Decrement"        |> Binding.cmd (fun state -> Decrement)
    "IncrementDelayed" |> Binding.cmd (fun state -> IncrementDelayed)
    "IncrementX10"     |> Binding.cmd (fun state -> IncrementTimes 10)
    "AsyncSeq"         |> Binding.cmd (fun state -> StartSeqMsgs)
    "NameMsg"          |> Binding.oneWay (fun state -> sprintf "Hello! Your counter is %d!" state.Count)
    "MyName"             |>  Binding.twoWay (fun state -> state.MyName) (fun name state -> NameIs name)
    "SayHello"         |> Binding.cmd (fun _ -> SayHello)
]



[<EntryPoint; STAThread>]
let main argv = 
    Program.mkProgram init update bindings
    |> Program.runWindow (mainWindow)