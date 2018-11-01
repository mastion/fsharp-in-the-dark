// Learn more about F# at http://fsharp.org

open System
open Blades.Core

[<EntryPoint>]
let main argv =
    let rollInfo : Types.ActionRoll = {
        position = Types.Position.Controlled
        attributeRank = 7
        effect = Types.Effect.Standard
    }
    printfn "Rolling %i with position %O and effect %O" rollInfo.attributeRank rollInfo.position rollInfo.effect
    let actionResult = Scoundrel.actionRoll rollInfo
    printfn "Roll result %O and with %i failures" actionResult.success actionResult.failures
    0 // return an integer exit code
