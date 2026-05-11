module PRo3D.Viewer.SimpleGui.App

open Aardvark.Base
open Aardvark.Rendering
open Aardvark.UI
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive

open PRo3D.Viewer.SimpleGui

module private SceneShaders =
    open Aardvark.Rendering.Effects
    open FShade

    type UniformScope with
        member x.UseSecondary : bool = uniform?UseSecondary
        member x.SecondaryOpacity : float32 = uniform?SecondaryOpacity
        member x.MousePos : V2f = uniform?MousePos
        member x.ViewportSize : V2f = uniform?ViewportSize
        member x.LensRadius : float32 = uniform?LensRadius
        member x.LensAsRectangle : bool = uniform?LensAsRectangle

    let private secondarySampler =
        sampler2d {
            texture uniform?SecondaryTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let stableTrafo (v : Vertex) =
        vertex {
            let vp = uniform.ModelViewTrafo * v.pos
            let wp = uniform.ModelTrafo * v.pos
            return {
                pos = uniform.ProjTrafo * vp
                wp = wp
                n = uniform.NormalMatrix * v.n
                b = uniform.NormalMatrix * v.b
                t = uniform.NormalMatrix * v.t
                c = v.c
                tc = v.tc
            }
        }

    let secondaryLens (v : Vertex) =
        fragment {
            let clip = v.pos
            let ndc = clip.XY / clip.W
            let fragPx = 
                V2f (
                    (ndc.X * 0.5f + 0.5f) * uniform.ViewportSize.X,
                    (ndc.Y * 0.5f + 0.5f) * uniform.ViewportSize.Y
                )

            let baseColor = v.c
            let secondaryColor = secondarySampler.Sample(v.tc)

            let mousePx =
                V2f(
                    uniform.MousePos.X,
                    uniform.ViewportSize.Y - uniform.MousePos.Y
                )

            let dx = mousePx.X - fragPx.X
            let dy = mousePx.Y - fragPx.Y

            let minViewportSize =
                if uniform.ViewportSize.X < uniform.ViewportSize.Y then
                    uniform.ViewportSize.X
                else
                    uniform.ViewportSize.Y

            let radiusPx =
                uniform.LensRadius * minViewportSize
            
            let halfWidthPx = radiusPx * 0.5f
            let halfHeightPx = radiusPx * 0.5f
    
            let insideLens =
                if uniform.LensAsRectangle then
                    abs(dx) < halfWidthPx && abs(dy) < halfHeightPx   
                else
                    dx * dx + dy * dy < radiusPx * radiusPx
                

            let secondaryMix =
                if uniform.UseSecondary || insideLens then
                    uniform.SecondaryOpacity
                else
                    0.0f

            let rgb =
                Fun.Lerp(secondaryMix, baseColor.XYZ, secondaryColor.XYZ)

            if insideLens then
                return V4f(rgb, 1.0f)
            else
                return V4f(rgb, baseColor.W)
        }


type Action =
    | SetFolder       of list<string>
    | CameraAction    of FreeFlyController.Message
    | ToggleSecondary
    | ToggleLodVis
    | ToggleFillMode
    | RecenterCamera
    | NextPrimaryTexture
    | PrevPrimaryTexture
    | SetPrimaryTextureLast
    | MoveCameraToPointOfInterest
    | NextSecondaryTexture
    | PrevSecondaryTexture
    | SetSecondaryOpacity of float32
    | SetLensRadius of float32
    | SetMouseAndViewPort of V2f * V2f
    | SetLensShapeRectangle of bool

type LoadOutcome =
    | Loaded of LoadedScene
    | Failed of string

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

let private freeFlyConfigForBox (bb : Box3d) : FreeFlyConfig =
    if bb.IsValid && not bb.IsEmpty && bb.Size.NormMax > 0.0 then
        let sceneSize = bb.Size.NormMax

        // Camera speed in world units per second.
        // Tune this multiplier if it feels too slow/fast.
        let unitsPerSecond =
            max 0.01 (sceneSize * 0.5)

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


let private cameraForBoxWithFreeFlyConfig (bb : Box3d) : CameraControllerState =
    { FreeFlyController.initial with
        view = cameraForBox bb
        freeFlyConfig = freeFlyConfigForBox bb
    }

/// Effects applied to the OPC scene. Exposed so the offscreen-screenshot
/// code path can reuse the same shader chain as the interactive view.
let sceneEffects : list<FShade.Effect> = [
    toEffect SceneShaders.stableTrafo
    toEffect DefaultSurfaces.diffuseTexture
    toEffect SceneShaders.secondaryLens
]

/// Build a default initial model. Optionally pre-load a folder
/// so the GUI starts up already showing a scene.
let initialModel (preload : Option<LoadedScene>) : Model =
    let bb = preload |> Option.map (fun s -> s.BoundingBox) |> Option.defaultValue Box3d.Invalid
    let near, far = nearFarForBox bb
    let textureCount = preload |> Option.map (fun s -> s.TextureCount) |> Option.defaultValue 1
    let initialPrimary = preload |> Option.map (fun s -> max 0 (s.TextureCount - 1)) |> Option.defaultValue 0
    let initialSecondary =
        if textureCount > 1 then
            (initialPrimary + 1) % textureCount
        else
            initialPrimary
    {
        loaded              = preload
        cameraState         = cameraForBoxWithFreeFlyConfig bb
        near                = near
        far                 = far
        primaryTextureIndex = initialPrimary
        useSecondary        = false
        secondaryOpacity    = 1.0f
        lodVisEnabled       = false
        secondaryTextureIndex = initialSecondary
        lensRadius          = 0.1f
        fillMode            = FillMode.Fill
        mousePos            = V2f(-1.0f, -1.0f)   // default: außerhalb / ungültig
        viewportSize =      V2f(1280.0f, 800.0f)     // default-Fallback
        lensAsRectangle     = true       
        statusMessage       =
            match preload with
            | Some s -> sprintf "Loaded %d hierarchies from %s (%d texture layers)" (List.length s.HierarchyPaths) s.RootDirectory s.TextureCount
            | None -> "Pick an OPC folder to load."
    }

/// Try loading an OPC directory; returns the resulting scene or an error message.
let tryLoadFolder (path : string) : LoadOutcome =
    try
        let basePaths = OpcLoading.findHierarchyBasePaths path
        if List.isEmpty basePaths then
            Failed (sprintf "no patchhierarchy.xml found under %s" path)
        else
            let hierarchies, bb = OpcLoading.loadHierarchies basePaths
            let sky = if Vec.length bb.Center > 0.0 then bb.Center.Normalized else V3d.OOI
            let pointsOfInterest = OpcLoading.loadPointsOfInterest path
            // any hierarchy will do — they should all share the same texture
            // layer layout. Fall back to 1 if something is off.
            let textureCount =
                hierarchies
                |> List.tryHead
                |> Option.map (fun (h, _) ->
                    OpcLoading.logTextureLayers h
                    OpcLoading.textureLayerCount h)
                |> Option.defaultValue 1
            Loaded { 
                RootDirectory = path
                HierarchyPaths = basePaths
                BoundingBox = bb
                Sky = sky
                TextureCount = textureCount
                PointsOfInterest = pointsOfInterest
            }
    with ex ->
        Failed (sprintf "load failed: %s" ex.Message)

let private wrapTextureIndex (count : int) (idx : int) =
    if count <= 0 then 0
    else ((idx % count) + count) % count

let update (m : Model) (a : Action) =
    match a with
    | CameraAction msg ->
        let newCamera = FreeFlyController.update m.cameraState msg
        logCamera newCamera
        { m with cameraState = newCamera }
    | SetFolder [] ->
        { m with statusMessage = "no folder chosen" }
    | SetFolder (path :: _) ->
        match tryLoadFolder path with
        | Loaded scene ->
            let near, far = nearFarForBox scene.BoundingBox
            let primary = max 0 (scene.TextureCount - 1)
            let secondary = wrapTextureIndex scene.TextureCount (primary + 1)
            { m with
                loaded = Some scene
                near = near
                far = far
                primaryTextureIndex = primary
                secondaryTextureIndex = secondary
                cameraState = cameraForBoxWithFreeFlyConfig scene.BoundingBox 
                statusMessage = sprintf "Loaded %d hierarchies from %s (%d texture layers)" (List.length scene.HierarchyPaths) path scene.TextureCount }
        | Failed msg ->
            { m with statusMessage = msg }
    | ToggleSecondary ->
        { m with useSecondary = not m.useSecondary }
    | ToggleLodVis ->
        { m with lodVisEnabled = not m.lodVisEnabled }
    | ToggleFillMode ->
        let next = match m.fillMode with FillMode.Fill -> FillMode.Line | _ -> FillMode.Fill
        { m with fillMode = next }
    | RecenterCamera ->
        match m.loaded with
        | Some s -> { m with cameraState = cameraForBoxWithFreeFlyConfig s.BoundingBox }
        | None -> m
    | NextPrimaryTexture ->
        match m.loaded with
        | Some s -> { m with primaryTextureIndex = wrapTextureIndex s.TextureCount (m.primaryTextureIndex + 1) }
        | None -> m
    | PrevPrimaryTexture ->
        match m.loaded with
        | Some s -> { m with primaryTextureIndex = wrapTextureIndex s.TextureCount (m.primaryTextureIndex - 1) }
        | None -> m
    | SetPrimaryTextureLast ->
        match m.loaded with
        | Some s -> { m with primaryTextureIndex = max 0 (s.TextureCount - 1) }
        | None -> m
    | MoveCameraToPointOfInterest ->
        match m.loaded with
        | Some scene -> 
           match scene.PointsOfInterest |> List.tryHead with
           | Some poi -> 
                { m with 
                    cameraState = 
                        { m.cameraState with 
                            view = cameraForPointOfInterest poi 
                        } 
                    statusMessage = sprintf "Moved camera to point of interest: %s" poi.Name
                 }
           | None ->
                { m with statusMessage = "No point-of-interest file found." }
        | None ->
            { m with statusMessage = "Load an OPC folder before moving to a point of interest." }
    | NextSecondaryTexture ->
        match m.loaded with
        | Some s -> { m with secondaryTextureIndex = wrapTextureIndex s.TextureCount (m.secondaryTextureIndex + 1) }
        | None -> m
    | PrevSecondaryTexture ->
        match m.loaded with
        | Some s -> { m with secondaryTextureIndex = wrapTextureIndex s.TextureCount (m.secondaryTextureIndex - 1) }
        | None -> m
    | SetSecondaryOpacity opacity ->
        { m with secondaryOpacity =  clamp 0.0f 1.0f opacity  }
    | SetLensRadius radius ->
        { m with lensRadius = clamp 0.01f 1.0f radius }
    | SetMouseAndViewPort (mouse, size) ->
        let safeSize =
            if size.X > 1.0f && size.Y > 1.0f then
                size
            else
                m.viewportSize

        { m with
            mousePos = mouse
            viewportSize = safeSize }
    | SetLensShapeRectangle rect ->
        { m with lensAsRectangle = rect }

/// Build the scene graph, wired up to all the toggle uniforms.
/// `buildScene` constructs the per-hierarchy SG using the captured runtime/runner.
let private buildSceneSg (buildScene : LoadedScene -> Aardvark.SceneGraph.ISg) (m : AdaptiveModel) : ISg<Action> =
    // primary texture index is only meaningful when a scene is loaded;
    // wrap it in Some so the AttributeParameters applicator sees a value.
    let primaryTexture : aval<Option<int>> =
        m.primaryTextureIndex |> AVal.map Some

    let secondaryTexture : aval<Option<int>> =
        m.secondaryTextureIndex |> AVal.map Some

    let opcSg : aval<ISg<Action>> =
        m.loaded |> AVal.map (function
            | Some scene ->
                buildScene scene
                |> OpcLoading.withPrimaryTextureIndex primaryTexture
                |> OpcLoading.withSecondaryTextureIndex secondaryTexture
                |> Sg.noEvents
            | None -> Sg.empty)

    Sg.dynamic opcSg
    |> Sg.effect sceneEffects
    |> Sg.uniform "UseSecondary" m.useSecondary
    |> Sg.uniform "SecondaryOpacity" m.secondaryOpacity
    |> Sg.uniform "MousePos" m.mousePos
    |> Sg.uniform "ViewportSize" m.viewportSize
    |> Sg.uniform "LensRadius" m.lensRadius
    |> Sg.uniform "LensAsRectangle" m.lensAsRectangle
    |> Sg.fillMode m.fillMode

let view (buildScene : LoadedScene -> Aardvark.SceneGraph.ISg) (m : AdaptiveModel) : DomNode<Action> =
    let frustum =
        AVal.map2 (fun n f -> Frustum.perspective 60.0 n f 1.0) m.near m.far

    let renderArea =
        FreeFlyController.controlledControl
            m.cameraState CameraAction frustum
            (AttributeMap.ofList [
                style "position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: 0"
                attribute "data-samples" "1"
                onEvent "onmousemove"
                       [
                            "(function(){var t=event.currentTarget;var r=t.getBoundingClientRect();return event.clientX-r.left;})()";
                            "(function(){var t=event.currentTarget;var r=t.getBoundingClientRect();return event.clientY-r.top;})()";
                            "(function(){var t=event.currentTarget;var r=t.getBoundingClientRect();return r.width;})()";
                            "(function(){var t=event.currentTarget;var r=t.getBoundingClientRect();return r.height;})()"
                        ]
                        (fun values ->
                            let x = System.Convert.ToSingle(values.[0])
                            let y = System.Convert.ToSingle(values.[1])
                            let w = System.Convert.ToSingle(values.[2])
                            let h = System.Convert.ToSingle(values.[3])
                            SetMouseAndViewPort (V2f(x, y), V2f(w, h)))
            ])
            (buildSceneSg buildScene m)

    let overlayStyle =
        "position: fixed; top: 8px; left: 8px; z-index: 10; \
         padding: 8px 10px; background: rgba(20,20,20,0.75); color: #eee; \
         font-family: sans-serif; border-radius: 4px"

    let labeledCheckbox (label : string) (current : aval<bool>) (msg : Action) =
        Incremental.div AttributeMap.empty (
            alist {
                let! isChecked = current
                let baseAttrs = [ attribute "type" "checkbox"; onClick (fun _ -> msg) ]
                let attrs = if isChecked then attribute "checked" "checked" :: baseAttrs else baseAttrs
                yield input attrs
                yield text (" " + label)
            }
        )

    let parseFloat32Invariant (s : string) =
        let clean =
            s.Trim()
             .Trim('"')
             .Trim('\'')

        match System.Double.TryParse(
            clean,
            System.Globalization.NumberStyles.Float,
            System.Globalization.CultureInfo.InvariantCulture
        ) with
        | true, v -> float32 v
        | _ -> 80.0f

    let toolbar =
        div [ style overlayStyle ] [
            div [] [
                openDialogButton
                    { OpenDialogConfig.folder with title = "Choose OPC root directory" }
                    [ clazz "ui green button"; onChooseFiles SetFolder ]
                    [ text "Open OPC folder" ]
            ]
            br []
            Incremental.div AttributeMap.empty (
                alist {
                    let! status = m.statusMessage
                    yield div [ style "margin-top: 6px; font-size: 12px; opacity: 0.85" ] [ text status ]
                }
            )
            br []
            div [] [ labeledCheckbox "show secondary texture" m.useSecondary ToggleSecondary ]
            div [] [ labeledCheckbox "LoD visualisation" m.lodVisEnabled ToggleLodVis ]
            div [] [
                Incremental.div AttributeMap.empty (
                    alist {
                        let! fm = m.fillMode
                        let isWire = (fm = FillMode.Line)
                        let baseAttrs = [ attribute "type" "checkbox"; onClick (fun _ -> ToggleFillMode) ]
                        let attrs = if isWire then attribute "checked" "checked" :: baseAttrs else baseAttrs
                        yield input attrs
                        yield text " wireframe"
                    }
                )
            ]
            div [ style "margin-top: 6px" ] [
                Incremental.div AttributeMap.empty (
                    alist {
                        let! loaded = m.loaded
                        let! idx    = m.primaryTextureIndex
                        let count = loaded |> Option.map (fun s -> s.TextureCount) |> Option.defaultValue 0
                        yield div [ style "font-size: 12px; margin-bottom: 2px" ]
                                  [ text (sprintf "primary texture: %d / %d" idx (max 0 (count - 1))) ]
                        yield button [ clazz "ui mini button"; onClick (fun _ -> PrevPrimaryTexture) ] [ text "<" ]
                        yield button [ clazz "ui mini button"; onClick (fun _ -> NextPrimaryTexture) ] [ text ">" ]
                        yield button [ clazz "ui mini button"; onClick (fun _ -> SetPrimaryTextureLast) ] [ text "albedo (last)" ]
                    }
                )
            ]
            div [ style "margin-top: 6px" ] [
                Incremental.div AttributeMap.empty (
                    alist {
                        let! loaded = m.loaded
                        let! idx = m.secondaryTextureIndex
                        let count = loaded |> Option.map (fun s -> s.TextureCount) |> Option.defaultValue 0

                        yield div [ style "font-size: 12px; margin-bottom: 2px" ]
                                  [ text (sprintf "secondary texture: %d / %d" idx (max 0 (count - 1))) ]

                        yield button [ clazz "ui mini button"; onClick (fun _ -> PrevSecondaryTexture) ] [ text "<" ]
                        yield button [ clazz "ui mini button"; onClick (fun _ -> NextSecondaryTexture) ] [ text ">" ]
                    }
                )
            ]
            button [ clazz "ui mini button"; onClick (fun _ -> SetSecondaryOpacity 0.25f) ] [ text "25%" ]
            button [ clazz "ui mini button"; onClick (fun _ -> SetSecondaryOpacity 0.50f) ] [ text "50%" ]
            button [ clazz "ui mini button"; onClick (fun _ -> SetSecondaryOpacity 1.00f) ] [ text "100%" ]      
            div [ style "margin-top: 6px" ] [
                Incremental.div AttributeMap.empty (
                    alist {
                        let! isRectangle = m.lensAsRectangle

                        let rectangleAttrs =
                            [
                                attribute "type" "radio"
                                attribute "name" "lens-shape"
                                attribute "value" "rectangle"
                                onClick (fun _ -> SetLensShapeRectangle true)
                            ]

                        let rectangleAttrs =
                            if isRectangle then
                                attribute "checked" "checked" :: rectangleAttrs
                            else
                                rectangleAttrs

                        let circleAttrs =
                            [
                                attribute "type" "radio"
                                attribute "name" "lens-shape"
                                attribute "value" "circle"
                                onClick (fun _ -> SetLensShapeRectangle false)
                            ]

                        let circleAttrs =
                            if not isRectangle then
                                attribute "checked" "checked" :: circleAttrs
                            else
                                circleAttrs

                        yield div [ style "font-size: 12px; margin-bottom: 2px" ] [
                            text "lens shape"
                        ]

                        yield label [ style "display: block; font-size: 12px" ] [
                            input rectangleAttrs
                            text " rectangle"
                        ]

                        yield label [ style "display: block; font-size: 12px" ] [
                            input circleAttrs
                            text " circle"
                        ]
                    }
                )
            ]
            div [ style "margin-top: 6px" ] [
                Incremental.div AttributeMap.empty (
                    alist {
                        let! radius = m.lensRadius

                        let radiusValue =
                            radius.ToString(
                                "0.00",
                                System.Globalization.CultureInfo.InvariantCulture
                            )

                        yield div [] [
                            div [ style "font-size: 12px; margin-bottom: 2px" ] [
                                text (sprintf "lens radius: %.0f%%" (radius * 100.0f))
                            ]

                            input [
                                attribute "type" "range"
                                attribute "min" "0.01"
                                attribute "max" "1"
                                attribute "step" "0.01"
                                attribute "value" radiusValue
                                style "width: 160px"

                                onEvent "oninput"
                                    [ "event.target.value" ]
                                    (fun values ->
                                        values.[0]
                                        |> parseFloat32Invariant
                                        |> SetLensRadius)
                            ]
                        ]
                    }
                )
            ]            
            div [ style "margin-top: 4px" ] [
                button [ clazz "ui mini button"; onClick (fun _ -> RecenterCamera) ] [ text "recenter" ]
            ]
            div [ style "margin-top: 4px" ] [
                button [ clazz "ui mini button"; onClick (fun _ -> MoveCameraToPointOfInterest) ] [ text "move to point of interest" ]
            ]
        ]

    body [ style "margin: 0; overflow: hidden; background: black" ] [
        renderArea
        toolbar
    ]

let threads (m : Model) =
    FreeFlyController.threads m.cameraState |> ThreadPool.map CameraAction

let app (preload : Option<LoadedScene>) (buildScene : LoadedScene -> Aardvark.SceneGraph.ISg) : App<Model, AdaptiveModel, Action> =
    {
        unpersist = Unpersist.instance
        threads   = threads
        initial   = initialModel preload
        update    = update
        view      = view buildScene
    }
