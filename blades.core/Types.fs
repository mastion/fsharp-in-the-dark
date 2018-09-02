namespace Blades.Core

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
