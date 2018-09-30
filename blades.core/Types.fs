namespace Blades.Core

open System.Runtime.CompilerServices
[<RequireQualifiedAccess>]
module Types =
    type ActionSuccesses = 
        | Critical
        | Full
        | Partial
        | Failure

    type ActionResult = 
        {
            success : ActionSuccesses
            failures : int
        }

    type Position =
        | Controlled
        | Risky
        | Desperate

    type Effect =
        | Limited
        | Standard
        | Great   

    type Attribute =
        | Attune of int
        | Command of int
        | Consort of int
        | Finesse of int
        | Hunt of int
        | Prowl of int
        | Skirmish of int
        | Study of int
        | Survey of int
        | Sway of int
        | Tinker of int
        | Wreck of int

    type ActionRoll = {
        position : Position;
        attributeRank : int; //todo put this in a type
        effect: Effect; 
    }

    type CritRule = ActionResult -> ActionResult