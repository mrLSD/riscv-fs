module Tests

open System
open Xunit

[<Fact>]
let ``My test`` () =
    Assert.True(true)
    Assert.Equal(3, 3)
