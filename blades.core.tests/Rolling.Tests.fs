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
    
    [<Fact>]
    let ``Should apply roll rules`` () =
        let rollInfo = { position = Types.Position.Desperate; attributeRank = 2; effect = Types.Effect.Limited } : Types.ActionRoll
        let simpleRule (actionRoll : Types.ActionRoll) =
            {
                position = Types.Position.Controlled
                attributeRank = actionRoll.attributeRank
                effect = actionRoll.effect
            } : Types.ActionRoll
        let rule : Types.ScoundrelRollRule = simpleRule
        let rules : Types.RuleList = [ (Types.ScoundrelRollRule simpleRule) ]
        let actual = applyRollRules rules rollInfo
        Assert.True(actual.position = Types.Position.Controlled)
        Assert.True(actual.attributeRank = 2)
        Assert.True(actual.effect = Types.Effect.Limited)

    [<Fact>]
    let ``Should apply roll rules`` () =
        let actionResult = { success = Types.ActionSuccesses.Failure; failures = 2;} : Types.ActionResult
        let simpleRule (actionResult : Types.ActionResult) =
            {
                success = Types.ActionSuccesses.Partial
                failures = 2
            } : Types.ActionResult
        let rule : Types.ScoundrelResultRule = simpleRule
        let rules : Types.RuleList = [ (Types.ScoundrelResultRule simpleRule) ]
        let actual = applyResultRules rules actionResult
        Assert.True(actual.success = Types.ActionSuccesses.Partial)
        Assert.True(actual.failures = 2)

    [<Fact>]
    let ``Should be able to roll a 6`` () =
        let mutable continueLooping = true
        let mutable loopedToManyTimes = 0
        let mutable hasA6 = false
        while continueLooping do
            if loopedToManyTimes = 100 then
                continueLooping <- false
            else
                let result = roll 100
                let maybe6 = (result |> List.tryFind( fun number -> number = 6))
                if maybe6.IsSome then
                    hasA6 <- true
                    continueLooping <- false
        Assert.True(hasA6)