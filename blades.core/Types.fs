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
            success : ActionSuccesses;
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
        | Attune
        | Command
        | Consort
        | Finesse
        | Hunt
        | Prowl
        | Skirmish
        | Study
        | Survey
        | Sway
        | Tinker
        | Wreck

    type ActionRoll = {
        position : Position;
        attributeRank : int; //todo put this in a type
        effect: Effect; 
    }

    type CritRule = ActionResult -> ActionResult