module Tests

open Xunit
open Xunit.Abstractions
open FsCheck
open FsCheck.Xunit
open Blades.Core
open Blades.Core.Scoundrel

type RealRolls =
    static member IntList() =
        let gen =
            gen { let! k = Gen.choose (2,10)
                  let! sizes = Gen.piles k 10
                  return! Gen.sequence [ for size in sizes -> Gen.resize size (Gen.choose (1,6)) ] }
        Arb.fromGen (gen)

type FailedRolls =
    static member IntList() =
        let gen =
            gen { let! k = Gen.choose (2,10)
                  let! sizes = Gen.piles k 10
                  return! Gen.sequence [ for size in sizes -> Gen.resize size (Gen.choose (1,3)) ] }
        Arb.fromGen (gen)


type RollingTests(output: ITestOutputHelper) =

    [<Property(Verbose = true, Arbitrary=[| typeof<RealRolls> |])>]
    let ``Should return the number of failed rolls no matter the overall action result`` (input : int list) =
        let actionRoll = { position = Types.Position.Controlled; attributeRank = input.Length; effect = Types.Effect.Standard} : Types.ActionRoll
        let expected = List.fold (fun acc elem -> if elem < 4 then acc + 1 else acc) 0 input
        let actual = Blades.Core.Scoundrel.rollToActionResult actionRoll input
        Assert.True(actual.failures = expected)

    [<Fact>]
    let ``Should return critial action result when more than one six is rolled`` () =
        let input = [6;6;1]
        let actionRoll = { position = Types.Position.Controlled; attributeRank = input.Length; effect = Types.Effect.Standard} : Types.ActionRoll        
        let expected = Types.ActionSuccesses.Critical
        let actual = Blades.Core.Scoundrel.rollToActionResult actionRoll input
        Assert.True(actual.success = expected)

    [<Fact>]
    let ``Should return success action result when one six is rolled`` () =
        let input = [6;5;1]
        let actionRoll = { position = Types.Position.Controlled; attributeRank = input.Length; effect = Types.Effect.Standard} : Types.ActionRoll        
        let expected = Types.ActionSuccesses.Full
        let actual = Blades.Core.Scoundrel.rollToActionResult actionRoll input
        Assert.True(actual.success = expected)

    [<Fact>]
    let ``Should return partial success action result when four is the highest roll`` () =
        let input = [4;4;1]
        let actionRoll = { position = Types.Position.Controlled; attributeRank = input.Length; effect = Types.Effect.Standard} : Types.ActionRoll        
        let expected = Types.ActionSuccesses.Partial
        let actual = Blades.Core.Scoundrel.rollToActionResult actionRoll input
        Assert.True(actual.success = expected)

    [<Fact>]
    let ``Should return partial success action result when five is the highest roll`` () =
        let input = [5;5;1]
        let actionRoll = { position = Types.Position.Controlled; attributeRank = input.Length; effect = Types.Effect.Standard} : Types.ActionRoll        
        let expected = Types.ActionSuccesses.Partial
        let actual = Blades.Core.Scoundrel.rollToActionResult actionRoll input
        Assert.True(actual.success = expected)

    [<Property(Verbose = true, Arbitrary=[| typeof<FailedRolls> |])>]
    let ``Should return failure success action result when no rolls are above 3`` (input : int list) =
        let actionRoll = { position = Types.Position.Controlled; attributeRank = input.Length; effect = Types.Effect.Standard} : Types.ActionRoll        
        let expected = Types.ActionSuccesses.Failure
        let actual = Blades.Core.Scoundrel.rollToActionResult actionRoll input
        Assert.True(actual.success = expected)
    
    [<Fact>]
    let ``A Roll of zero dice should result in two die being rolled`` () =
        let rolledDice = Scoundrel.roll 0
        Assert.True(rolledDice.Length = 2)

    [<Fact>]
    let ``Should not crit if 0 die were rolled`` () =
        let diceRolled = [6;6]
        let actionRoll = { position = Types.Position.Controlled; attributeRank = 0; effect = Types.Effect.Standard} : Types.ActionRoll
        let actual = rollToActionResult actionRoll diceRolled
        Assert.True(actual.success = Types.ActionSuccesses.Full)

    [<Fact>]
    let ``Should return failure if lesser roll is below threshold for 0 attributerank`` () =
        let diceRolled = [5;3]
        let actionRoll = {position = Types.Position.Controlled; attributeRank = 0; effect = Types.Effect.Standard}: Types.ActionRoll
        let actual = rollToActionResult actionRoll diceRolled
        Assert.True(actual.success = Types.ActionSuccesses.Failure)