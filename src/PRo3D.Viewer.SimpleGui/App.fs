module PRo3D.Viewer.SimpleGui.App

open Aardvark.Base
open Aardvark.Rendering
open Aardvark.UI
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive

open PRo3D.Viewer.SimpleGui

type Action =
    | SetFolder       of list<string>
    | CameraAction    of FreeFlyController.Message
    | ToggleLens
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
    | SetTransferFunctionMode of TransferFunctionMode
    | SetTextureCombiner of TextureCombiner
    | SetTFBlendFactor of float32
    | SetTFRange of V2f
    | SetTransferFunctionColorMap of string

type LoadOutcome =
    | Loaded of LoadedScene
    | Failed of string

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
    let near, far = Camera.nearFarForBox bb
    let textureCount = preload |> Option.map (fun s -> s.TextureCount) |> Option.defaultValue 1
    let initialPrimary = preload |> Option.map (fun s -> max 0 (s.TextureCount - 1)) |> Option.defaultValue 0
    let initialSecondary =
        if textureCount > 1 then
            (initialPrimary + 1) % textureCount
        else
            initialPrimary
    {
        loaded              = preload
        cameraState         = Camera.cameraForBoxWithFreeFlyConfig bb
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
        viewportSize        = V2f(1280.0f, 800.0f)     // default-Fallback
        lensAsRectangle     = true   
        transferFunctionMode = TransferFunctionMode.Passthrough // default: input = output
        textureCombiner = TextureCombiner.None  // default: do not combine the textures, but show secondary only
        TFBlendFactor = 0.5f
        TFRange = V2f(0.0f, 1.0f)
        transferFunctionColorMap = "plasma"
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
            let hierarchies, bb, textureCount =
                OpcLoading.loadHierarchiesWithMoreThanOneTexture basePaths

            if List.isEmpty hierarchies then
                Failed (sprintf "no hierarchies with more than one texture layer found under %s" path)
            else
                let loadedPaths =
                    hierarchies |> List.map snd

                loadedPaths
                |> List.iter (fun p ->
                    Log.line "[App] Actually loaded hierarchy: %s" p
                )

                let sky =
                    if Vec.length bb.Center > 0.0 then
                        bb.Center.Normalized
                    else
                        V3d.OOI

                let pointsOfInterest =
                    OpcLoading.loadPointsOfInterest path

                let wavelengthConfig =
                    OpcLoading.loadWavelengthConfig path

                Loaded { 
                    RootDirectory = path
                    HierarchyPaths = loadedPaths
                    BoundingBox = bb
                    Sky = sky
                    TextureCount = textureCount
                    PointsOfInterest = pointsOfInterest
                    WavelengthConfig = wavelengthConfig
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
        //logCamera newCamera
        { m with cameraState = newCamera }
    | SetFolder [] ->
        { m with statusMessage = "no folder chosen" }
    | SetFolder (path :: _) ->
        match tryLoadFolder path with
        | Loaded scene ->
            let near, far = Camera.nearFarForBox scene.BoundingBox
            let primary = max 0 (scene.TextureCount - 1)
            let secondary = wrapTextureIndex scene.TextureCount (primary + 1)
            let loadedPaths =
                scene.HierarchyPaths 

            loadedPaths
            |> List.iter (fun p ->
                Log.line "[App] Loaded hierarchy: %s" p
            )
            { m with
                loaded = Some scene
                near = near
                far = far
                primaryTextureIndex = primary
                secondaryTextureIndex = secondary
                cameraState = Camera.cameraForBoxWithFreeFlyConfig scene.BoundingBox 
                statusMessage = sprintf "Loaded %d hierarchies from %s (%d texture layers)" (List.length scene.HierarchyPaths) path scene.TextureCount }
        | Failed msg ->
            { m with statusMessage = msg }
    | ToggleLens ->
        { m with useSecondary = not m.useSecondary }
    | ToggleLodVis ->
        { m with lodVisEnabled = not m.lodVisEnabled }
    | ToggleFillMode ->
        let next = match m.fillMode with FillMode.Fill -> FillMode.Line | _ -> FillMode.Fill
        { m with fillMode = next }
    | RecenterCamera ->
        match m.loaded with
        | Some s -> { m with cameraState = Camera.cameraForBoxWithFreeFlyConfig s.BoundingBox }
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
                            view = Camera.cameraForPointOfInterest poi 
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
    | SetTransferFunctionMode mode ->
        { m with transferFunctionMode = mode }
    | SetTextureCombiner combiner ->
            { m with textureCombiner = combiner }
    | SetTFBlendFactor factor ->
            { m with TFBlendFactor = clamp 0.0f 1.0f factor }
    | SetTFRange range ->
            let safeRange =
                if range.X >= 0.0f && range.Y <= 1.0f && range.X <= range.Y then
                    range
                else
                    V2f(0.0f, 1.0f)
            { m with TFRange = safeRange }
    | SetTransferFunctionColorMap name ->
            { m with transferFunctionColorMap = name }

let private transferFunctionTexture (name : string) =
    match PRo3D.Base.ColorMaps.colorMaps |> Map.tryFind name with
    | Some tex ->
        tex.Value
    | None ->    // default
        PRo3D.Base.ColorMaps.colorMaps
        |> Map.find "plasma"
        |> fun tex -> tex.Value

let private transferFunctionColorMapImagePath (name : string) =
    sprintf "resources/%s.png" name

// needs a rework when working with real data wavelengths
let private wavelengthRangeForTextureIndex
        (config : WavelengthConfig)
        (textureIndex : int)
        : Option<int * int * string> =

    let values =
        config.Wavelengths |> List.toArray

    // Texture index 0 -> wavelengths[0], wavelengths[1]
    // Texture index 1 -> wavelengths[2], wavelengths[3]
    let startIndex =
        textureIndex * 2

    if startIndex + 1 < values.Length then
        Some (values.[startIndex], values.[startIndex + 1], config.Unit)
    else
        None

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

    let selectTransferFunctionTexture =
        m.transferFunctionColorMap |> AVal.map transferFunctionTexture  

    Sg.dynamic opcSg
    |> Sg.effect sceneEffects
    |> Sg.uniform "UseSecondary" m.useSecondary
    |> Sg.uniform "SecondaryOpacity" m.secondaryOpacity
    |> Sg.uniform "MousePos" m.mousePos
    |> Sg.uniform "ViewportSize" m.viewportSize
    |> Sg.uniform "LensRadius" m.lensRadius
    |> Sg.uniform "LensAsRectangle" m.lensAsRectangle
    |> Sg.uniform "TransferFunctionMode" (m.transferFunctionMode |> AVal.map (fun mode -> int mode))
    |> Sg.uniform "TextureCombiner" (m.textureCombiner |> AVal.map (fun mode -> int mode))
    |> Sg.uniform "TFBlendFactor" m.TFBlendFactor
    |> Sg.uniform "TFRange" m.TFRange
    |> Sg.texture "SecondaryTextureTransferFunction" selectTransferFunctionTexture
    |> Sg.fillMode m.fillMode

/// savely converts a string to a float32, independent of the computer language settings
let private parseEventFloat32 (s : string) =
    System.Single.Parse(
        s.Trim().Trim('"').Trim('\''),
        System.Globalization.NumberStyles.Float,
        System.Globalization.CultureInfo.InvariantCulture
    )

let view (buildScene : LoadedScene -> Aardvark.SceneGraph.ISg) (m : AdaptiveModel) : DomNode<Action> =
    let frustum =
        AVal.map2 (fun n f -> Frustum.perspective 60.0 n f 1.0) m.near m.far

    let renderArea =
        FreeFlyController.controlledControl
            m.cameraState CameraAction frustum
            (AttributeMap.ofList [
                style Styles.renderArea
                attribute "data-samples" "1"
                onEvent "onmousemove"
                       [
                            "(function(){var t=event.currentTarget;var r=t.getBoundingClientRect();return event.clientX-r.left;})()";
                            "(function(){var t=event.currentTarget;var r=t.getBoundingClientRect();return event.clientY-r.top;})()";
                            "(function(){var t=event.currentTarget;var r=t.getBoundingClientRect();return r.width;})()";
                            "(function(){var t=event.currentTarget;var r=t.getBoundingClientRect();return r.height;})()"
                        ]
                        (fun values ->
                            let x = parseEventFloat32 values.[0]
                            let y = parseEventFloat32 values.[1]
                            let w = parseEventFloat32 values.[2]
                            let h = parseEventFloat32 values.[3]
                            SetMouseAndViewPort (V2f(x, y), V2f(w, h)))
            ])
            (buildSceneSg buildScene m)

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

    let toolbar =
        div [ style Styles.overlayToolbar ] [
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
            
            div [ style "margin-top: 4px" ] [
                button [ clazz "ui mini button"; onClick (fun _ -> RecenterCamera) ] [ text "recenter" ]
            ]
            div [ style "margin-top: 4px" ] [
                button [ clazz "ui mini button"; onClick (fun _ -> MoveCameraToPointOfInterest) ] [ text "move to point of interest" ]
            ]
            br []
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
            br []
            div [] [
                Incremental.div AttributeMap.empty (
                    alist {
                        let! lensActive = m.useSecondary

                        let lensAttrs =
                            [
                                attribute "type" "radio"
                                attribute "name" "lens-active"
                                attribute "value" "lens-active"
                                onClick (fun _ ->  ToggleLens)
                            ]
                            
                        let lensAttrs =
                            if lensActive then
                                attribute "checked" "checked" :: lensAttrs
                            else
                                lensAttrs

                        yield label [ style "display: block; font-size: 12px" ] [
                            input lensAttrs
                            text " lens active"
                        ]


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
                                        match values.[0] |> Wavelength.tryParseFloat32Invariant with
                                        | Some radius -> SetLensRadius radius
                                        | None -> SetLensRadius 1.0f)  // fallback: max radius when parsing fails
                            ]
                        ]
                    }
                )
            ]            
            div [ style "margin-top: 4px" ] [
                Incremental.div AttributeMap.empty (
                    alist {
                            let! current = m.textureCombiner

                            let noneAttr =
                                [
                                    attribute "type" "radio"
                                    attribute "name" "none"
                                    attribute "value" "none"
                                    onClick(fun _ -> SetTextureCombiner TextureCombiner.None)
                                ]

                            let noneAttr =
                                if current = TextureCombiner.None then
                                    attribute "checked" "checked" :: noneAttr
                                else    
                                    noneAttr

                            let multiplyAttr =
                                [
                                    attribute "type" "radio"
                                    attribute "name" "multiply"
                                    attribute "value" "multiply"
                                    onClick(fun _ -> SetTextureCombiner TextureCombiner.Multiply)
                                ]

                            let multiplyAttr =
                                if current = TextureCombiner.Multiply then
                                    attribute "checked" "checked" :: multiplyAttr
                                else    
                                    multiplyAttr

                            let blendAttr =
                                [
                                    attribute "type" "radio"
                                    attribute "name" "blend"
                                    attribute "value" "blend"
                                    onClick(fun _ -> SetTextureCombiner TextureCombiner.Blend)
                                ]

                            let blendAttr =
                                if current = TextureCombiner.Blend then
                                    attribute "checked" "checked" :: blendAttr
                                else    
                                    blendAttr

                            yield div [ style "font-size: 12px; margin-bottom: 2px" ] [
                                text "texture combiner: "
                            ]

                            yield label [ style "display: block; font-size: 12px" ] [
                                input noneAttr
                                text " none"
                            ]

                            yield label [ style "display: block; font-size: 12px" ] [
                                input multiplyAttr
                                text " multiply"
                            ]

                            yield label [ style "display: block; font-size: 12px" ] [
                                input blendAttr
                                text " blend"
                            ]

                        }
                    )
    
                div [ style "margin-top: 4px" ] [
                    Incremental.div AttributeMap.empty (
                        alist {
                            let! current = m.transferFunctionMode

                            let passthroughAttr =
                                [
                                    attribute "type" "radio"
                                    attribute "name" "transfer-function"
                                    attribute "value" "passthrough"
                                    onClick(fun _ -> SetTransferFunctionMode TransferFunctionMode.Passthrough)
                                ]

                            let passthroughAttr = 
                                if current = TransferFunctionMode.Passthrough then
                                    attribute "checked" "checked" :: passthroughAttr
                                else 
                                    passthroughAttr

                            let rampAttrs = 
                                [
                                    attribute "type" "radio"
                                    attribute "name" "transfer-function"
                                    attribute "value" "ramp"
                                    onClick(fun _ -> SetTransferFunctionMode TransferFunctionMode.Ramp)
                                ]

                            let rampAttrs =
                                if current = TransferFunctionMode.Ramp then
                                    attribute "checked" "checked" :: rampAttrs
                                else 
                                    rampAttrs

                            yield div [ style "font-size: 12px; margin-bottom: 2px" ] [
                                text "transfer function mode"
                            ]

                            yield label [ style "display: block; font-size: 12px" ] [
                                input passthroughAttr
                                text " passthrough"
                            ]

                            yield label [ style "display: block; font-size: 12px" ] [
                                input rampAttrs
                                text " ramp"
                            ]

                        }
                    )
                ]
                div [ style "margin-top: 6px" ] [
                    Incremental.div AttributeMap.empty (
                        alist {
                            let! selectedColorMap = m.transferFunctionColorMap

                            let plasmaAttrs =
                                [
                                    attribute "type" "radio"
                                    attribute "name" "transfer-function-colormap"
                                    attribute "value" "plasma"
                                    onClick (fun _ -> SetTransferFunctionColorMap "plasma")
                                ]

                            let plasmaAttrs =
                                if selectedColorMap = "plasma" then
                                    attribute "checked" "checked" :: plasmaAttrs
                                else
                                    plasmaAttrs

                            let orangesAttrs =
                                [
                                    attribute "type" "radio"
                                    attribute "name" "transfer-function-colormap"
                                    attribute "value" "oranges"
                                    onClick (fun _ -> SetTransferFunctionColorMap "oranges")
                                ]

                            let orangesAttrs =
                                if selectedColorMap = "oranges" then
                                    attribute "checked" "checked" :: orangesAttrs
                                else
                                    orangesAttrs

                            let spectralAttrs =
                                [
                                    attribute "type" "radio"
                                    attribute "name" "transfer-function-colormap"
                                    attribute "value" "spectral"
                                    onClick (fun _ -> SetTransferFunctionColorMap "spectral")
                                ]

                            let spectralAttrs =
                                if selectedColorMap = "spectral" then
                                    attribute "checked" "checked" :: spectralAttrs
                                else
                                    spectralAttrs

                            yield div [ style "font-size: 12px; margin-bottom: 2px" ] [
                                text "transfer function color map"
                            ]

                            yield label [ style "display: block; font-size: 12px" ] [
                                input plasmaAttrs
                                text " plasma"
                            ]

                            yield label [ style "display: block; font-size: 12px" ] [
                                input orangesAttrs
                                text " oranges"
                            ]

                            yield label [ style "display: block; font-size: 12px" ] [
                                input spectralAttrs
                                text " spectral"
                            ]
                        }
                    )
                ]

                div [ style Styles.wavelengthRangeSection ] [
                    Incremental.div AttributeMap.empty (
                        alist {
                            let! loaded = m.loaded
                            let! useSecondary = m.useSecondary
                            let! primaryIndex = m.primaryTextureIndex
                            let! secondaryIndex = m.secondaryTextureIndex
                            let! tfRange = m.TFRange

                            let activeTextureIndex =
                                if useSecondary then
                                    secondaryIndex
                                else
                                    primaryIndex

                            match loaded with
                            | Some scene ->
                                match wavelengthRangeForTextureIndex scene.WavelengthConfig activeTextureIndex with
                                | Some (minNm, maxNm, unit) ->

                                    let left01 =
                                        clamp 0.0f 1.0f tfRange.X

                                    let right01 =
                                        clamp 0.0f 1.0f tfRange.Y

                                    let selectedMinNm =
                                        Wavelength.value01ToWavelength minNm maxNm left01

                                    let selectedMaxNm =
                                        Wavelength.value01ToWavelength minNm maxNm right01

                                    let minValue =
                                        Wavelength.formatIntInvariant minNm

                                    let maxValue =
                                        Wavelength.formatIntInvariant maxNm

                                    let selectedMinValue =
                                        Wavelength.formatFloat32Invariant selectedMinNm

                                    let selectedMaxValue =
                                        Wavelength.formatFloat32Invariant selectedMaxNm

                                    yield div [] [
                                        div [ style Styles.wavelengthRangeTitle ] [
                                            text (
                                                sprintf
                                                    "wavelength range:"
                                            )
                                        ]

                                        div [ style Styles.wavelengthRangeLimitLabel ] [
                                            span [] [ text (sprintf "min %d %s" minNm unit) ]
                                            span [] [ text (sprintf "max %d %s" maxNm unit) ]
                                        ]

                                        div [ 
                                            style Styles.wavelengthRangeInputRow
                                            attribute "data-wavelength-range-row" "true"
                                        ] [
                                            input [
                                                attribute "type" "number"
                                                attribute "min" minValue
                                                attribute "max" maxValue
                                                attribute "step" "1"
                                                attribute "placeholder" "min"
                                                attribute "value" selectedMinValue
                                                style Styles.wavelengthRangeInput

                                                onEvent "onchange"
                                                    [ Wavelength.wavelengthMinInputValueScript minNm maxNm selectedMaxNm ]
                                                    (fun values ->
                                                        let value = values.[0]

                                                        if value = Wavelength.fullWavelengthRangeToken then
                                                            SetTFRange Wavelength.fullWavelengthRange01
                                                        else
                                                            match value |> Wavelength.tryParseFloat32Invariant with
                                                            | Some newMinNm ->
                                                                let newRange =
                                                                    Wavelength.wavelengthSelectionToRangeOrFull minNm maxNm newMinNm selectedMaxNm

                                                                SetTFRange newRange

                                                            | None ->
                                                                SetTFRange Wavelength.fullWavelengthRange01)
                                            ]

                                            div [ 
                                                style Styles.wavelengthRangeInputRow
                                            ] [
                                                input [
                                                    attribute "type" "number"
                                                    attribute "min" minValue
                                                    attribute "max" maxValue
                                                    attribute "step" "1"
                                                    attribute "placeholder" "max"
                                                    attribute "value" selectedMaxValue
                                                    style Styles.wavelengthRangeInput

                                                    onEvent "onchange"
                                                        [ Wavelength.wavelengthMaxInputValueScript minNm maxNm selectedMinNm ]
                                                        (fun values ->
                                                            let value = values.[0]

                                                            if value = Wavelength.fullWavelengthRangeToken then
                                                                SetTFRange Wavelength.fullWavelengthRange01
                                                            else
                                                                match value |> Wavelength.tryParseFloat32Invariant with
                                                                | Some newMaxNm ->
                                                                    let newRange =
                                                                        Wavelength.wavelengthSelectionToRangeOrFull minNm maxNm selectedMinNm newMaxNm

                                                                    SetTFRange newRange

                                                                | None ->
                                                                    SetTFRange Wavelength.fullWavelengthRange01)
                                                ]

                                                span [ style Styles.wavelengthRangeUnit ] [
                                                    text (sprintf " %s" unit)
                                                ]
                                            ]
                                        ]
                                    ]

                                | None ->
                                    yield div [ style Styles.wavelengthRangeWarning ] [
                                        text (sprintf "no wavelength range for texture %d" activeTextureIndex)
                                    ]

                            | None ->
                                yield div [ style Styles.wavelengthRangeMuted ] [
                                    text "load a scene to select wavelength range"
                                ]
                        }
                    )
                ]
            ]
        ]


    let colorMapOverlay =
        div [ style Styles.overlayLegend ] [
            Incremental.div (
                AttributeMap.ofList [
                ]
            )  (
                alist {
                    let! loaded = m.loaded
                    let! selectedColorMap = m.transferFunctionColorMap
                    let! useSecondary = m.useSecondary
                    let! primaryIndex = m.primaryTextureIndex
                    let! secondaryIndex = m.secondaryTextureIndex
                    let! tfRange = m.TFRange

                    let activeTextureIndex =
                        if useSecondary then
                            secondaryIndex
                        else
                            primaryIndex


                    match loaded with
                    | Some scene ->
                        match wavelengthRangeForTextureIndex scene.WavelengthConfig activeTextureIndex with
                        | Some (minNm, maxNm, unit) ->
                            let left01 =
                                clamp 0.0f 1.0f tfRange.X

                            let right01 =
                                clamp 0.0f 1.0f tfRange.Y

                            let imagePath =
                                transferFunctionColorMapImagePath selectedColorMap

                            let rangeText =
                                match loaded with
                                | Some scene ->
                                    match wavelengthRangeForTextureIndex scene.WavelengthConfig activeTextureIndex with
                                    | Some (minNm, maxNm, unit) ->
                                        Some (minNm, maxNm, unit)
                                    | None ->
                                        None
                                | None ->
                                    None

                            yield div [] [
                                yield div [style " font-size: 12px; margin-bottom: 4px;" ] [
                                    text (sprintf "colormap: %s" selectedColorMap)
                                ]

                                yield img [
                                    attribute "src" imagePath
                                    style "width: 100%; height: 18px; display: block; image-rendering: auto;"
                                ]
                       
                                match rangeText with
                                | Some (minNm, maxNm, unit) ->
                                    yield div [
                                        style Styles.overlayColorMapLabel
                                    ] [
                                        span [] [
                                            text (sprintf "min wavelength %d %s" minNm unit)
                                        ]

                                        span [] [
                                            text (sprintf "max wavelength %d %s" maxNm unit)
                                        ]
                                    ]

                                | None ->
                                    yield div [
                                        style Styles.overlayColorMapLabel
                                    ] [
                                        text (sprintf "no wavelength range for texture %d" activeTextureIndex)
                                    ]

                                let selectedMinPercentage = Wavelength.formatFloat32Invariant left01
                                let selectedMaxPercentage = Wavelength.formatFloat32Invariant right01

                                yield div [
                                        style Styles.overlayColorMapLabel
                                    ] [
                                        span [] [
                                            text (sprintf "selected min %s" selectedMinPercentage)
                                        ]

                                        span [] [
                                            text (sprintf "selected max %s" selectedMaxPercentage)
                                        ]
                                    ]
                            ]

                        | None ->
                            yield div [ style Styles.wavelengthRangeWarning ] [
                                text (sprintf "no wavelength range for texture %d" activeTextureIndex)
                            ]
                    | None ->
                        yield div [ style Styles.wavelengthRangeMuted ] [
                            text "load a scene to select wavelength range"
                        ]

                }
            )
        ]

    body [ style "margin: 0; overflow: hidden; background: black" ] [
        renderArea
        toolbar
        colorMapOverlay
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