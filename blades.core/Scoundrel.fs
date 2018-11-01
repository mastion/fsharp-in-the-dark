namespace Blades.Core

module Scoundrel =

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