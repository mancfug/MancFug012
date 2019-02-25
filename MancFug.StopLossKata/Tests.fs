module Tests

open System
open Xunit

type Event =
    | PriceHeldAt of int * DateTime

type Command =
    | StockPriceChanges of int * DateTime

module Handler =
    let stockPriceChange (givenHistory : Event list) (change,date) = 
      []

let execute (givenHistory : Event list) (command : Command) : Event list =
  match command with
  | StockPriceChanges (change,date) -> Handler.stockPriceChange givenHistory (change, date)
    
let Given (events : Event list) = events

let When command history =
  match execute history command with
  | [] -> None
  | events -> Some events

let eventEquals expected actual =
  set expected = set actual

let ThenExpect (expectedEvents:Event list) (actualEvents:Event list option) =
  match actualEvents with
  | Some (actualEvents) ->
      Assert.Equal<Event list>(expectedEvents, actualEvents)
  | None ->
      Assert.Empty(expectedEvents)

let baseLine = DateTime.Now

[<Fact>]
let ``Stock price increase within 15 seconds does not change held price`` () =
    Given [ PriceHeldAt (10, baseLine) ]
    |> When (StockPriceChanges (11, baseLine.AddSeconds(10.0)))
    |> ThenExpect []