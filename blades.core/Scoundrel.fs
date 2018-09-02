namespace Blades.Core

module Scoundrel =

    let internal roll amount =
        let rnd = System.Random()
        List.init amount (fun _ -> (rnd.Next(1,6)))

    let internal rollToActionResult rollList = 
        rollList
        |> List.fold (fun acc roll ->
                                    match roll with
                                    | 6 -> 
                                        match acc.success with
                                        | Types.ActionSuccesses.Critical ->
                                            acc
                                        | Types.ActionSuccesses.Full ->
                                            {success= Types.ActionSuccesses.Critical; failures=acc.failures}
                                        | _ ->
                                            {success= Types.ActionSuccesses.Full; failures=acc.failures}
                                    | 5 | 4 ->
                                        match acc.success with
                                        | Types.ActionSuccesses.Critical | Types.ActionSuccesses.Full ->
                                            acc
                                        | _ ->
                                            {success= Types.ActionSuccesses.Partial; failures=acc.failures}
                                    | _ ->
                                            {success= acc.success; failures=acc.failures+1}
                                    )
                                    {success= Types.ActionSuccesses.Failure; failures=0} : Types.ActionResult

(*
   let internal rollToActionResult rollList = 
        let countFailures list = (List.fold (fun acc elem -> if elem < 4 then acc + 1 else acc) 0 list)
        let countFullSuccesses list = (List.fold (fun acc elem -> if elem = 6 then acc + 1 else acc) 0 list)
        rollList
        |> List.sort
        |> (fun l -> 
                    if (countFullSuccesses l) > 1 then
                        {success=Types.ActionSuccesses.Critical; failures=countFailures l} : Types.ActionResult
                    else if List.contains 6 l then
                        {success=Types.ActionSuccesses.Full; failures=countFailures l} : Types.ActionResult
                    else if List.contains 4 l || List.contains 5 l then
                        {success=Types.ActionSuccesses.Partial; failures=countFailures l} : Types.ActionResult
                    else
                        {success=Types.ActionSuccesses.Failure; failures=countFailures l} : Types.ActionResult
                        )
*)

    let actionRoll statCount =
        roll statCount
        |> rollToActionResult