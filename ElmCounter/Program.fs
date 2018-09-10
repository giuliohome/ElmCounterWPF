module App 

open Elmish
open Elmish.WPF
open System
open FsXaml
open System.Windows.Threading
open System.ComponentModel
open FsXaml
open System
open System.Collections.Generic
open System.Windows

type State = { Count: int; State: string; MyName: string; HelloMsg: string; mutable CloseEvtMap: Dictionary<int, List<Action>> } 

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
    | NewBurrito
    | SubscribeClose of List<Action>
    | SubmitBurrito of Object

let rnd = Random()

let helloMsg name count = sprintf "Hello %s! Your counter is %d!" name count //state.MyName state.Count
let initName = "Mister X"
let mutable countBurrito = 0
let init() = 
    let startDict = new Dictionary<int,List<Action>>()
    startDict.Add(countBurrito, new List<Action>())
    { Count = 0; State="Ready"; MyName= initName; HelloMsg = helloMsg initName 0; CloseEvtMap = startDict}, Cmd.none

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

type HelloWindow = XAML<"SecondWindow.xaml">
type MainWindow = XAML<"MainWindow.xaml"> 
type BurritoWindow = XAML<"ProduceBurrito.xaml">

let mainWindow = MainWindow()

let showWin () =     
            mainWindow.Dispatcher.Invoke(fun () -> 
            let helloWin = HelloWindow()
            helloWin.DataContext <- mainWindow.DataContext
            helloWin.Show()) 

let burritoWin () =     
            mainWindow.Dispatcher.Invoke(fun () -> 
            let burritoWin = BurritoWindow()
            burritoWin.DataContext <- mainWindow.DataContext
            burritoWin.Show()) 
 


let submitBurrito (list: List<Action>) = 
    list.ForEach(fun a -> mainWindow.Dispatcher.Invoke(fun () ->  a.Invoke() ))

let update msg state =  
    match msg with 
    | Increment -> 
        let newCount = state.Count + 1
        let nextState = { state with Count = newCount; State="Incremented by 1"; HelloMsg = helloMsg state.MyName newCount }
        nextState, Cmd.none 
        
    | IncrementMofN (m, n) -> 
        let newCount = state.Count + 1
        let nextState = { state with Count = newCount; State= sprintf "Incremented by %d of %d" m n; HelloMsg = helloMsg state.MyName newCount }
        nextState, Cmd.none 

    | Decrement -> 
        let newCount = state.Count - 1
        let nextState = { state with Count = newCount; State="Decremented by 1"; HelloMsg = helloMsg state.MyName newCount}
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
        let newCount = state.Count + msg.Adder
        {state with Count = newCount; State= sprintf "%d iterations to go" msg.ToGo; HelloMsg = helloMsg state.MyName newCount }, Cmd.ofMsg (IncrementTimes msg.ToGo)

    | StartSeqMsgs -> state, asyncSeqEvery 1000 ([1..3] |> List.map (fun i -> IncrementMofN (i, 3)))

    | SayHello ->    
        state, Cmd.attemptFunc  showWin () (fun ex -> ShowMsg ex.Message)  
    
    | NameIs myName -> { state with MyName = myName; HelloMsg = helloMsg myName state.Count }, Cmd.ofMsg  ShowName
    | ShowName -> {state with State= sprintf "Hello %s" state.MyName }, Cmd.none 
    
    | NewBurrito ->    
        state, Cmd.attemptFunc  burritoWin () (fun ex -> ShowMsg ex.Message)  
    | SubscribeClose list -> 
        state.CloseEvtMap.Add(countBurrito, list)
        countBurrito <- countBurrito + 1
        state, Cmd.none
    | SubmitBurrito obj -> 
        let list = obj :?> List<Action>
        state, Cmd.attemptFunc submitBurrito list (fun ex -> ShowMsg ex.Message)

let bindings model dispatch = [
    
    "Count"            |> Binding.oneWay (fun state -> state.Count)
    "State"            |> Binding.oneWay (fun state -> state.State)
    "Increment"        |> Binding.cmd (fun state -> Increment)
    "Decrement"        |> Binding.cmd (fun state -> Decrement)
    "IncrementDelayed" |> Binding.cmd (fun state -> IncrementDelayed)
    "IncrementX10"     |> Binding.cmd (fun state -> IncrementTimes 10)
    "AsyncSeq"         |> Binding.cmd (fun state -> StartSeqMsgs)
    "NameMsg"          |> Binding.oneWay (fun state -> state.HelloMsg)
    "MyName"           |>  Binding.twoWay (fun state -> state.MyName) (fun name _ -> NameIs name)
    "SayHello"         |> Binding.cmd (fun _ -> SayHello)
    "ProduceBurrito"   |> Binding.cmd (fun _ -> NewBurrito)
    "CloseEvtList"      |> Binding.twoWay
                            (fun s -> 
                                countBurrito <- countBurrito + 1
                                s.CloseEvtMap.Add(countBurrito, new List<Action>())
                                s.CloseEvtMap.Item countBurrito)
                            (fun list s -> SubscribeClose list)
    "SaveBurrito"      |> Binding.paramCmd (fun obj state -> 
                                    SubmitBurrito obj )
]



[<EntryPoint; STAThread>]
let main argv = 
    Program.mkProgram init update bindings
    |> Program.runWindow (mainWindow)