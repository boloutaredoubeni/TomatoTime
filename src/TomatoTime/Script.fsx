#!/usr/bin/env fsharpi

// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library.fs"
#r "../../packages/Colorful.Console/lib/net45/Colorful.Console.dll"
open TomatoTime
open System
open System.Drawing
open Colorful
let num = Library.hello 42
printfn "%i" num

// https://github.com/akkadotnet/akka.net/blob/dev/src/core/Akka.FSharp/FsApi.fs
type Agent<'T> = MailboxProcessor<'T>

type TimerCommand =
  | Start
  | Stop
  | Now

exception InvalidUserInputException of UserInput: string

type ClientStatus =
  | ClientIsAlive
  | ClientIsDead
  | Undetermined

type AppCommand =
  | ClientInputError of InvalidInput: string
  | GetClientStatus of AsyncReplyChannel<ClientStatus>
  | RestartClient of ClientConstructor:(unit -> Agent<string>) * AsyncReplyChannel<Agent<string>>
  | FatalError of exn

let supervisor exceptionHandler =
  let agent =
    new Agent<AppCommand>(fun inbox ->
      do printfn "Staring the supervisor"
      let rec loop clientStatus =
        async {
          let! message = inbox.Receive ()
          match message with
          | ClientInputError malformedInput -> 
            do printfn "'%s' is not valid input" malformedInput
            do! loop ClientIsDead
          | GetClientStatus replyChannel ->
            replyChannel .Reply clientStatus
            do! loop clientStatus
          | RestartClient (cons,  replyChannel) ->
            let newClient = cons ()
            replyChannel .Reply newClient
            do! loop ClientIsAlive
          | FatalError exn ->
            do printfn "Supervisor received an exception: %A" exn
            raise exn 
        }
      loop Undetermined)
  agent .Error .Add exceptionHandler
  agent .Start ()
  agent

let pomodoroAgent =
  Agent<TimerCommand>.Start(fun inbox ->
    do printfn "Starting the pomodoro timer"
    let rec loop () =
      async {
        let! message = inbox.Receive ()
        match message with
        | Start -> do! loop ()
        | Stop -> return ()
        | Now ->
          do printfn "Tomato Time"
          do! loop ()
        do! loop ()
      }
    loop ())

let clientAgent (supervisor: Agent<_>) =
  let agent =
    new Agent<string>(fun inbox ->
      do printfn "Starting a client agent"
      let rec loop () =
        do printfn "Enter a command see what happens"
        async {
          let! message = inbox.Receive ()
          match message with
          | "1" | "Start" ->
            do pomodoroAgent .Post Start
            do! loop ()
          | "2" | "Stop" ->
            do pomodoroAgent .Post Stop
            do! loop ()
          | "3" | "Now" ->
            do pomodoroAgent .Post Now
            do! loop ()
          | _ -> 
            do
              printfn "A client agent is stoping\n\tA new one will start soon\n\tEnter a command"
              raise (InvalidUserInputException message)
        }
      loop ())
  do
    agent .Error .Add (function 
      | InvalidUserInputException message -> (supervisor .Post << ClientInputError) message
      | exn -> (supervisor .Post << FatalError) exn)
    agent .Start ()
  agent

let main (_: array<string>) =
  do printfn "Starting the application"
  let clientSupervisor = supervisor raise
  let mutable client = clientAgent clientSupervisor
  let rec readUserInput () =
    let clientStatusReply = 
      (Async.RunSynchronously << clientSupervisor .PostAndTryAsyncReply) GetClientStatus
    match clientStatusReply with
    | None -> failwith "Supervisor timed out"
    | Some clientStatus ->
      if clientStatus = ClientIsDead
        then
          do printfn "Would you like to continue? [Yn]"
          let input = Console .ReadLine ()
          match input .ToUpper () with
          | "" | "Y"  -> do client <- clientAgent clientSupervisor
          | _ -> do printfn "Exiting..."
      let command = Console.ReadLine ()
      do
        client .Post command
        readUserInput ()
  readUserInput ()
  0

do main [||] |> ignore

// FIXME: use actual system time
// FIXME: Use NodaTime to print time on demand
// FIXME: Do the pomodoro
