module PRo3D.Viewer.SimpleGui.Program

open System
open System.IO

open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Application
open Aardvark.Application.Slim

// note: do NOT `open Aardvark.UI` at file scope — its `Sg` module shadows
// `Aardvark.SceneGraph.Sg`, breaking `Sg.ofList` over plain `ISg`.

open Aardvark.GeoSpatial.Opc.Load

open Suave
open Suave.WebPart
open Aardium

open MBrace.FsPickler
open Aardvark.Data.Opc
open Aardvark.GeoSpatial.Opc

open FSharp.Data.Adaptive

/// Args parsed from argv. Either runs interactively (Aardium) or renders a
/// single screenshot to disk (no Aardium, exits when done).
type private Args = {
    folder        : Option<string>
    screenshot    : Option<string>
    width         : int
    height        : int
    samples       : int
    logFile       : Option<string>
    /// Optional override for the primary-texture LegacyId in screenshot
    /// mode. Defaults to "last layer" (= albedo) when not given.
    textureIndex  : Option<int>
}

let private defaultArgs = {
    folder = None; screenshot = None
    width = 1280; height = 800; samples = 4; logFile = None
    textureIndex = None
}

let rec private parseArgs (acc : Args) (xs : list<string>) =
    match xs with
    | [] -> acc
    | "--screenshot" :: out :: rest -> parseArgs { acc with screenshot = Some out } rest
    | "--width"  :: w :: rest -> parseArgs { acc with width = int w } rest
    | "--height" :: h :: rest -> parseArgs { acc with height = int h } rest
    | "--samples" :: s :: rest -> parseArgs { acc with samples = int s } rest
    | "--log"    :: p :: rest -> parseArgs { acc with logFile = Some p } rest
    | "--texture" :: i :: rest -> parseArgs { acc with textureIndex = Some (int i) } rest
    | path :: rest when acc.folder = None -> parseArgs { acc with folder = Some path } rest
    | unknown :: rest ->
        Log.warn "[SimpleGui] ignoring unknown arg: %s" unknown
        parseArgs acc rest

/// Build the SG for one LoadedScene. `asyncLoading=false` is required for
/// screenshot mode so the first rendered frame already has all patches.
let private buildSceneFor
        (signature    : IFramebufferSignature)
        (runner       : Load.Runner)
        (asyncLoading : bool) : LoadedScene -> ISg =
    let serializer = FsPickler.CreateBinarySerializer()
    fun scene ->
        scene.HierarchyPaths
        |> List.map (fun bp ->
            let h =
                Aardvark.Data.Opc.PatchHierarchy.load
                    serializer.Pickle serializer.UnPickle (OpcPaths.OpcPaths bp)
                                
            OpcLoading.buildHierarchySg signature runner asyncLoading bp h)
        |> Sg.ofList

/// Renders the scene once to an offscreen framebuffer and writes it as PNG.
/// Uses synchronous LOD loading so the single rendered frame is fully populated.
let private runScreenshot
        (runtime    : IRuntime)
        (scene      : LoadedScene)
        (outPath    : string)
        (width      : int)
        (height     : int)
        (samples    : int)
        (textureIdx : Option<int>) =

    let signature =
        runtime.CreateFramebufferSignature(
            [| DefaultSemantic.Colors, TextureFormat.Rgba8
               DefaultSemantic.DepthStencil, TextureFormat.Depth24Stencil8 |],
            samples = samples)

    let runner = runtime.CreateLoadRunner 1
    let buildScene = buildSceneFor signature runner false

    let near, far = App.nearFarForBox scene.BoundingBox
    let view = App.cameraForBox scene.BoundingBox
    let aspect = float width / float height
    let frustum = Frustum.perspective 60.0 near far aspect

    // default to the last layer = real albedo for typical OPC datasets
    let primaryIdx =
        textureIdx |> Option.defaultValue (max 0 (scene.TextureCount - 1))
    Log.line "[SimpleGui] using primary texture index %d (of %d)" primaryIdx scene.TextureCount

    let sg =
        buildScene scene
        |> OpcLoading.withPrimaryTextureIndex (AVal.constant (Some primaryIdx))
        |> Sg.viewTrafo  (view |> CameraView.viewTrafo |> AVal.constant)
        |> Sg.projTrafo  (frustum |> Frustum.projTrafo |> AVal.constant)
        |> Sg.effect     App.sceneEffects
        |> Sg.uniform'   "UseSecondary" false
        |> Sg.fillMode'  FillMode.Fill

    use task = runtime.CompileRender(signature, sg)

    let size = AVal.constant (V2i(width, height))
    let clear =
        clear {
            color  C4f.Black
            depth  1.0
            stencil 0
        }

    let colorTex = task |> RenderTask.renderToColorWithClear size clear

    let pix = runtime.Download(colorTex.GetValue() :?> IBackendTexture)
    let dir = Path.GetDirectoryName outPath
    if not (String.IsNullOrEmpty dir) && not (Directory.Exists dir) then
        Directory.CreateDirectory dir |> ignore
    pix.Save(outPath)
    Log.line "[SimpleGui] wrote screenshot: %s (%dx%d, %dxAA)" outPath width height samples

[<EntryPoint; STAThread>]
let main argv =
    let args = parseArgs defaultArgs (Array.toList argv)

    // redirect Aardvark log to a custom file when requested (handy for
    // putting test artifacts next to the screenshot).
    match args.logFile with
    | Some path ->
        let dir = Path.GetDirectoryName path
        if not (String.IsNullOrEmpty dir) && not (Directory.Exists dir) then
            Directory.CreateDirectory dir |> ignore
        Aardvark.Base.Report.LogFileName <- path
    | None -> ()

    Aardvark.Init()

    use app = new OpenGlApplication()
    let runtime = app.Runtime :> IRuntime

    match args.screenshot, args.folder with
    | Some out, Some folder ->
        match App.tryLoadFolder folder with
        | App.Loaded scene ->
            runScreenshot runtime scene out args.width args.height args.samples args.textureIndex
            0
        | App.Failed msg ->
            Log.warn "[SimpleGui] screenshot: cannot load %s — %s" folder msg
            1
    | Some _, None ->
        eprintfn "--screenshot requires a folder path. Usage: --screenshot <out.png> <opc-folder>"
        2
    | None, _ ->
        // interactive mode: Aardium + suave
        Aardium.init()

        let signature =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, TextureFormat.Rgba8
                DefaultSemantic.DepthStencil, TextureFormat.Depth24Stencil8
            ]
        let runner = runtime.CreateLoadRunner 1
        let buildScene = buildSceneFor signature runner true

        let preload =
            match args.folder with
            | Some path when Directory.Exists path ->
                match App.tryLoadFolder path with
                | App.Loaded scene -> 
                    let serializer = FsPickler.CreateBinarySerializer()
                   
                    let hPs = 
                        scene.HierarchyPaths
                        |> List.filter (fun bp -> 
                            let h = Aardvark.Data.Opc.PatchHierarchy.load serializer.Pickle serializer.UnPickle (OpcPaths.OpcPaths bp)
                            (OpcLoading.textureLayerCount h) > 1)

                    Some ({scene with HierarchyPaths = hPs})
                | App.Failed msg ->
                    Log.warn "[SimpleGui] preload failed: %s" msg
                    None
            | _ -> None

        let mediaApp = App.app preload buildScene
        let instance = mediaApp.start()

        WebPart.startServerLocalhost 4321 [
            Aardvark.UI.MutableApp.toWebPart' runtime false instance
            Suave.Files.browseHome
        ] |> ignore

        Aardium.run {
            url "http://localhost:4321/"
            width 1280
            height 800
            debug true
        }
        0
