module PRo3D.Viewer.SimpleGui.Camera

open Aardvark.Base
open Aardvark.Rendering
open Aardvark.UI
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive

open PRo3D.Viewer.SimpleGui

let cameraForBox (bb : Box3d) : CameraView =
    if bb.IsValid && not bb.IsEmpty && bb.Size.NormMax > 0.0 then
        let sky = if Vec.length bb.Center > 0.0 then bb.Center.Normalized else V3d.OOI
        CameraView.lookAt bb.Max bb.Center sky
    else
        CameraView.lookAt (V3d(3.0, 3.0, 3.0)) V3d.Zero V3d.OOI

// this can be used to find a location for creating a poiint of interest file
let private logCamera (cameraState : CameraControllerState) =
    let view = cameraState.view
    let p = view.Location
    let f = view.Forward
    let u = view.Up

    Log.line
        "[camera] pos=(%.6f, %.6f, %.6f), forward=(%.6f, %.6f, %.6f), up=(%.6f, %.6f, %.6f)"
        p.X p.Y p.Z
        f.X f.Y f.Z
        u.X u.Y u.Z

let cameraForPointOfInterest (poi : PointOfInterestCamera) : CameraView =
    let forward =
        if Vec.length poi.Forward > 1e-8 then
            poi.Forward.Normalized
        else
            V3d.OOI

    let up =
        if Vec.length poi.Up > 1e-8 then
            poi.Up.Normalized
        else
            V3d.OOI

    let target = poi.Position + forward

    CameraView.lookAt poi.Position target up

let nearFarForBox (bb : Box3d) : float * float =
    if bb.IsValid && not bb.IsEmpty && bb.Size.NormMax > 0.0 then
        let s = bb.Size.NormMax
        max 0.01 (s * 0.001), s * 100.0
    else
        0.1, 1000.0

let freeFlyConfigForBox (bb : Box3d) : FreeFlyConfig =
    if bb.IsValid && not bb.IsEmpty && bb.Size.NormMax > 0.0 then
        let sceneSize = bb.Size.NormMax

        // Camera speed in world units per second.
        // Tune this multiplier if it feels too slow/fast.
        let unitsPerSecond =
            max 0.01 (sceneSize * 0.25)

        let heuristic =
            FreeFlyHeuristics.DefaultSpeedHeuristic(
                0.0,
                FreeFlyConfig.initial
            )

        let adjusted : FreeFlyHeuristics.SpeedHeuristic =
            heuristic.AdjustToUnitsPerSecond unitsPerSecond

        adjusted.Config
    else
        FreeFlyConfig.initial


let cameraForBoxWithFreeFlyConfig (bb : Box3d) : CameraControllerState =
    { FreeFlyController.initial with
        view = cameraForBox bb
        freeFlyConfig = freeFlyConfigForBox bb
    }


