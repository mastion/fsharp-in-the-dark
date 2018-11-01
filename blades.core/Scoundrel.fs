namespace Blades.Core

type ScoundrelType = { rules: Types.RuleList}

module Scoundrel =

    let create rules = 
        {rules = rules}

    let internal roll amount =
        let actualAmount amount =
            if amount = 0 then
                2
            else
                amount

        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let rnd = System.Random()
        stopWatch.Stop()
        printfn " ---- Creating Random object took: %f" stopWatch.Elapsed.TotalMilliseconds
        
        stopWatch.Reset()
        stopWatch.Start()
        let numbers = List.init (actualAmount amount) (fun _ -> (rnd.Next(1,7)))
        stopWatch.Stop()
        numbers
        |> List.iter (fun n -> printfn "%i" n)

        printfn " ------ Genearting random numbers: %f" stopWatch.Elapsed.TotalMilliseconds
        numbers

    let internal cleanseForAttribute0 (rollInfo : Types.ActionRoll) rollList =
        if rollInfo.attributeRank = 0 then [List.min rollList] else rollList

    let internal critRule : Types.CritRule = fun actionResult ->
            match actionResult.success with
            | Types.ActionSuccesses.Critical ->
                actionResult
            | Types.ActionSuccesses.Full ->
                {success= Types.ActionSuccesses.Critical; failures=actionResult.failures}
            | _ ->
                {success= Types.ActionSuccesses.Full; failures=actionResult.failures} : Types.ActionResult

    let internal rollToActionResult (rollInfo : Types.ActionRoll) rollList =
        rollList
        |> cleanseForAttribute0 rollInfo
        |> List.fold (fun acc roll ->
                                    match roll with
                                    | 6 ->
                                        printfn "Six"
                                        critRule acc
                                    | 5 | 4 ->
                                        printfn "Five - Four"
                                        match acc.success with
                                        | Types.ActionSuccesses.Full ->
                                            acc
                                        | _ ->
                                            {success= Types.ActionSuccesses.Partial; failures=acc.failures}
                                    | _ ->
                                            printfn "Three - Two - One"
                                            {success= acc.success; failures=acc.failures+1}
                                    )
                                    {success= Types.ActionSuccesses.Failure; failures=0} : Types.ActionResult

    let actionRoll (rollInfo : Types.ActionRoll) = //TODO: read through SRD to 
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let numberList = roll rollInfo.attributeRank
        stopWatch.Stop()
        printfn " ------ NumberList Generation: %f" stopWatch.Elapsed.TotalMilliseconds
        stopWatch.Reset()
        stopWatch.Start()
        let result = numberList |> (rollToActionResult rollInfo)
        stopWatch.Stop()
        printfn " ------ rollResult: %f" stopWatch.Elapsed.TotalMilliseconds
        stopWatch.Reset()
        result

    let applyRollRules  (rules: Types.RuleList) (rollInfo : Types.ActionRoll) =
        let result : List<Types.ScoundrelRollRule> = List.empty
        rules
        |> (List.fold (fun acc rule -> 
                                        match rule with
                                        | Types.ScoundrelRollRule r -> acc @ [r]
                                        | Types.ScoundrelResultRule r-> acc ) result )
        |> (List.fold (fun acc rule -> (rule acc)) rollInfo)

    let applyResultRules  (rules: Types.RuleList) (rollResult : Types.ActionResult) =
        let result : List<Types.ScoundrelResultRule> = List.empty
        rules
        |> (List.fold (fun acc rule -> 
                                        match rule with
                                        | Types.ScoundrelRollRule r -> acc
                                        | Types.ScoundrelResultRule r-> acc @ [r] ) result )
        |> (List.fold (fun acc rule -> (rule acc)) rollResult)

    let actionRollWithRulesApplied (rollInfo : Types.ActionRoll) (rules: Types.RuleList) =
        rollInfo
        |> 
        ((applyRollRules rules)
        >> actionRoll
        >> (applyResultRules rules)) 