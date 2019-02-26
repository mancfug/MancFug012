module Tests

open System
open Xunit

type Event =
    | PriceHeldAt of int * DateTime

type Command =
    | StockIsBought of int * DateTime
    | StockPriceChanges of int * DateTime

module Handler =
    let stockBought (change, date) = [ PriceHeldAt (change, date)]
    let stockPriceChange (givenHistory : Event list) (change,date) = 
      []

let execute (givenHistory : Event list) (command : Command) : Event list =
  match command with
  | StockIsBought (change,date) -> Handler.stockBought (change, date)
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
let ``Stock is bought`` () =
  Given []
  |> When (StockIsBought (10, baseLine))
  |> ThenExpect [PriceHeldAt (10, baseLine)]
