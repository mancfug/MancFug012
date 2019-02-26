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
    | CheckIfPriceShouldBeHeld of int * DateTime

module Projections =
    let minimumPriceInAfter (givenHistory : Event list) earliestDate =
      givenHistory 
      |> Seq.choose (fun e -> // could be a function here, if i can figure the syntax
        match e with
        | PriceChanged (change,date) when date >= earliestDate -> Some change
        | _ -> None )
      |> Seq.min

module Handler =
    let stockBought (price, date) = [
      PriceChanged (price, date) 
      PriceHeldAt (price, date) ]
    let stockPriceChange (newPrice,date:DateTime) = [
      PriceChanged (newPrice, date) 
      CheckPriceHeldLogged(newPrice, date.AddSeconds 15.0)]
    let checkIfPriceHeld (givenHistory : Event list) (priceToHold,date:DateTime) = 
      let minimumPriceInLast15Seconds = Projections.minimumPriceInAfter givenHistory (date.AddSeconds -15.0)
      if minimumPriceInLast15Seconds < priceToHold then
        []
      else
        [
          PriceHeldAt (priceToHold, date) ]

let execute (givenHistory : Event list) (command : Command) : Event list =
  match command with
  | StockIsBought (originalPrice,date) -> Handler.stockBought (originalPrice, date)
  | StockPriceChanges (changedPrice,date) -> Handler.stockPriceChange (changedPrice, date)
  | CheckIfPriceShouldBeHeld (holdPrice,date) -> Handler.checkIfPriceHeld givenHistory (holdPrice, date)
    
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

[<Fact>]
let ``price should be held if it has changed in over 15 seconds`` () =
  Given [PriceChanged (10, baseLine)]
  |> When (CheckIfPriceShouldBeHeld (10, baseLine.AddSeconds 15.0))
  |> ThenExpect [ PriceHeldAt (10, baseLine.AddSeconds 15.0) ]

[<Fact>]
let ``price should not be held if it dips within 15 seconds`` () =
  Given [PriceChanged (9, baseLine)]
  |> When (CheckIfPriceShouldBeHeld (10, baseLine.AddSeconds 1.0))
  |> ThenExpect [ ]
