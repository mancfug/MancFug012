module Tests

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Hello world`` () =
    Trade.greet "World" |> should equal "Hello, World!"
