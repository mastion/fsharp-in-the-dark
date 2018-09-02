module Tests

open Xunit
open Xunit.Abstractions
open Blades.Core

let parseTestInput (input: string) =
    input.Split(',')
    |> Array.map System.Int32.Parse
    |> Seq.toList

type RollingTests(output: ITestOutputHelper) =
    //todo: should replace this less good idea with the good idea property tests
    // https://fsharpforfunandprofit.com/posts/property-based-testing-2/
    // https://github.com/fscheck/FsCheck

    [<Theory>]
    //can't seem to make a list of ints so apparently i am going to pass in a strin until I can figure something else out
    [<InlineData("6,6,6,6,6")>] 
    [<InlineData("6,6,5,1")>]
    let ``Should return critial action result when more than one six is rolled`` (inputAsString:string) =
        let input = parseTestInput inputAsString

        let expected = Types.ActionSuccesses.Critical
        let actual = Blades.Core.Scoundrel.rollToActionResult input
        Assert.True(actual.success = expected)

    [<Fact>]
    let ``Should return success action result when one six is rolled`` () =
        let input = [6;5;1]

        let expected = Types.ActionSuccesses.Full
        let actual = Blades.Core.Scoundrel.rollToActionResult input
        Assert.True(actual.success = expected)

    [<Theory>]
    [<InlineData("4,4,4")>]
    [<InlineData("5,5,1")>]
    [<InlineData("5,4,1")>]
    let ``Should return partial success action result when any number of fours or fives are rolled`` inputAsString =
        let input = parseTestInput inputAsString

        let expected = Types.ActionSuccesses.Partial
        let actual = Blades.Core.Scoundrel.rollToActionResult input
        Assert.True(actual.success = expected)

    [<Theory>]
    [<InlineData("3,3")>]
    [<InlineData("1,2,3")>]
    [<InlineData("2,2,2,2")>]
    let ``Should return failure success action result when no rolls are above 3`` inputAsString =
        let input = parseTestInput inputAsString

        let expected = Types.ActionSuccesses.Failure
        let actual = Blades.Core.Scoundrel.rollToActionResult input
        Assert.True(actual.success = expected)

    [<Theory>]
    [<InlineData("3,3,6")>]
    [<InlineData("1,2,3,5,5")>]
    [<InlineData("2,2,2,2,1")>]
    member __.``Should return the number of failled rolls no matter the overall action result`` inputAsString =
        let input = parseTestInput inputAsString

        let expected = List.fold (fun acc elem -> if elem < 4 then acc + 1 else acc) 0 input
        let actual = Blades.Core.Scoundrel.rollToActionResult input
        Assert.True(actual.failures = expected)
