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

        let rnd = System.Random()
        List.init (actualAmount amount) (fun _ -> (rnd.Next(1,6)))

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
                                        critRule acc
                                    | 5 | 4 ->
                                        match acc.success with
                                        | Types.ActionSuccesses.Full ->
                                            acc
                                        | _ ->
                                            {success= Types.ActionSuccesses.Partial; failures=acc.failures}
                                    | _ ->
                                            {success= acc.success; failures=acc.failures+1}
                                    )
                                    {success= Types.ActionSuccesses.Failure; failures=0} : Types.ActionResult

    let actionRoll (rollInfo : Types.ActionRoll) = //TODO: read through SRD to 
        roll rollInfo.attributeRank
        |> (rollToActionResult rollInfo)

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