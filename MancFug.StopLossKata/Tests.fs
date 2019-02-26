module Tests

open System
open Xunit

type Event =
    | PriceChanged of int * DateTime
    | PriceHeldAt of int * DateTime
    | CheckPriceHeldLogged of int * DateTime

type Command =
    | StockIsBought of int * DateTime
    | StockPriceChanges of int * DateTime
    | CheckIfPriceShouldBeHeld

module Handler =
    let stockBought (change, date) = [
      PriceChanged (change, date) 
      PriceHeldAt (change, date) ]
    let stockPriceChange (change,date:DateTime) = [
      PriceChanged (change, date) 
      CheckPriceHeldLogged(change, date.AddSeconds 15.0)]

let execute (givenHistory : Event list) (command : Command) : Event list =
  match command with
  | StockIsBought (change,date) -> Handler.stockBought (change, date)
  | StockPriceChanges (change,date) -> Handler.stockPriceChange (change, date)
  | CheckIfPriceShouldBeHeld -> []
    
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
  |> ThenExpect [
    PriceChanged (10, baseLine) 
    PriceHeldAt (10, baseLine) ]

[<Fact>]
let ``when price changes our future selves should check if it should be held`` () =
  Given [PriceHeldAt (10, baseLine)]
  |> When (StockPriceChanges (11, baseLine.AddSeconds 10.0))
  |> ThenExpect [
    PriceChanged (11, baseLine.AddSeconds 10.0)
    CheckPriceHeldLogged (11, baseLine.AddSeconds 25.0) ]

