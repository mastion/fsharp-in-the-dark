// Learn more about F# at http://fsharp.org

open System
open Blades.Core

[<EntryPoint>]
let main argv =
    let rollInfo : Types.ActionRoll = {
        position = Types.Position.Controlled
        attributeRank = 12
        effect = Types.Effect.Standard
    }
    printfn "Rolling %i with position %O and effect %O" rollInfo.attributeRank rollInfo.position rollInfo.effect
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let actionResult = Scoundrel.actionRoll rollInfo
    stopWatch.Stop()
    printfn "Roll result %O and with %i failures" actionResult.success actionResult.failures
    printfn "Roll took: %f" stopWatch.Elapsed.TotalMilliseconds
    0 // return an integer exit code
