#!/usr/bin/env fsharpi

// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library.fs"
open TomatoTime
open System
let num = Library.hello 42
printfn "%i" num

type TimerCommand =
    | Start
    | Stop
    | Now // of AsyncReplyChannel

// https://github.com/akkadotnet/akka.net/blob/dev/src/core/Akka.FSharp/FsApi.fs
type Agent<'T> = MailboxProcessor<'T>
let (<!) (agent: Agent<_>) message = agent .Post message
let (<?) (agent: Agent<_>) message = agent.PostAndAsyncReply

type AgentBuilder(?state) = class end

let pomodoroAgent =
  Agent<TimerCommand>.Start(fun inbox ->
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

let clientAgent =
  Agent<string>.Start(fun inbox ->
    let rec loop () =
      async {
        let! message = inbox.Receive ()
        match message with
        | "1" | "Start" ->
          do pomodoroAgent <! Start
          do! loop ()
        | "2" | "Stop" ->
          do pomodoroAgent <! Stop
          do! loop ()
        | "3" | "Now" ->
          do pomodoroAgent <! Now
          do! loop ()
        | _ -> failwithf "Invalid input: %s" message
      }
    loop ())

let main (_: array<string>) =
  let rec readUserInput () =
    let command = Console.ReadLine ()
    do
      clientAgent <! command
      readUserInput ()
  readUserInput ()
  0

do main [||] |> ignore

// FIXME: restart client after failure
// FIXME: use actual system time
// FIXME: Use NodaTime to print time on demand
// FIXME: Do the pomodoro
