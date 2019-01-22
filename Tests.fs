module Tests

open Xunit
open FsUnit.Xunit

[<Fact>]
let ``True is true`` () =
    true |> should be True
