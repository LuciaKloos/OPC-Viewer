namespace PRo3D.Viewer.SimpleGui

open System.IO

open Aardvark.Base
open Aardvark.Rendering
open Aardvark.SceneGraph
open Aardvark.Data.Opc
open Aardvark.GeoSpatial.Opc
open Aardvark.GeoSpatial.Opc.Configurations
open Aardvark.GeoSpatial.Opc.Load
open System.Text.Json

open FSharp.Data.Adaptive
open MBrace.FsPickler


module OpcLoading =

    type PoiDto() =
        member val Name : string = null with get, set
        member val Position : float[] = null with get, set
        member val Forward : float[] = null with get, set
        member val Up : float[] = null with get, set

    type PoiFileDto() =
        member val PointsOfInterest : PoiDto[] = null with get, set

    type WavelengthConfigDto() =
        member val Unit : string = "nm" with get, set
        member val Wavelengths : int[] = [||] with get, set
        
    let private tryV3d (values : float[]) =
        if isNull values || values.Length <> 3 then
            None
        else
            Some (V3d(values.[0], values.[1], values.[2]))

    let loadPointsOfInterest (rootDir : string) : list<PointOfInterestCamera> =    
        let path =
            Path.Combine(   
                rootDir,  
                "points-of-interest.json"
            )

        if not (File.Exists path) then
            Log.warn "[POI] File does not exist: %s" path
            []
        else
            try
                Log.line "[POI] File found: %s" path

                let json = File.ReadAllText path

                let options = JsonSerializerOptions()
                options.PropertyNameCaseInsensitive <- true

                let file = JsonSerializer.Deserialize<PoiFileDto>(json, options)

                if isNull file then
                    Log.warn "[POI] JSON root could not be deserialized."
                    []
                elif isNull file.PointsOfInterest then
                    Log.warn "[POI] JSON does not contain a PointsOfInterest array."
                    []
                else
                    Log.line "[POI] Found %d raw points of interest." file.PointsOfInterest.Length

                    file.PointsOfInterest
                    |> Array.choose (fun p ->
                        match tryV3d p.Position, tryV3d p.Forward, tryV3d p.Up with
                        | Some pos, Some forward, Some up ->
                            Some ({
                                Name =
                                    if System.String.IsNullOrWhiteSpace p.Name then
                                        "point of interest"
                                    else
                                        p.Name
                                Position = pos
                                Forward = forward
                                Up = up
                            } : PointOfInterestCamera)
                        | _ ->
                            Log.warn "[POI] Ignoring invalid point of interest in %s" path
                            None
                    )
                    |> Array.toList
            with ex ->
                Log.warn "[POI] Could not read %s: %s" path ex.Message
                []

    let loadWavelengthConfig (rootDir : string) : WavelengthConfig =
        let fallback =
            {
                Unit = "nm"
                Wavelengths = [ 400; 600; 1700; 1800 ]
            }

        let path =
            Path.Combine(rootDir, "resources\wavelengths.json")

        if not (File.Exists path) then
            Log.warn "[Wavelengths] File does not exist: %s. Using fallback." path
            fallback
        else
            try
                let json = File.ReadAllText path

                let options = JsonSerializerOptions()
                options.PropertyNameCaseInsensitive <- true

                let file =
                    JsonSerializer.Deserialize<WavelengthConfigDto>(json, options)

                if isNull file || isNull file.Wavelengths || file.Wavelengths.Length < 4 then
                    Log.warn "[Wavelengths] Invalid wavelength config. Using fallback."
                    fallback
                else
                    {
                        Unit =
                            if System.String.IsNullOrWhiteSpace file.Unit then
                                "nm"
                            else
                                file.Unit

                        Wavelengths =
                            file.Wavelengths |> Array.toList
                    }
            with ex ->
                Log.warn "[Wavelengths] Could not read %s: %s. Using fallback." path ex.Message
                fallback

    let private serializer = FsPickler.CreateBinarySerializer()

    /// Recursively find every directory that owns a `patches/patchhierarchy.xml`
    /// (i.e. the base path expected by `PatchHierarchy.load`).
    let findHierarchyBasePaths (rootDir : string) : list<string> =
        if not (Directory.Exists rootDir) then []
        else
            let root = DirectoryInfo(rootDir)
            root.EnumerateDirectories("patches", SearchOption.AllDirectories)
            |> Seq.filter (fun d -> File.Exists(Path.Combine(d.FullName, "patchhierarchy.xml")))
            |> Seq.choose (fun d -> if isNull d.Parent then None else Some d.Parent.FullName)
            |> Seq.distinct
            |> List.ofSeq

    let private rootPatch (h : PatchHierarchy) : Patch =
        match h.tree with
        | QTree.Node (n, _) -> n
        | QTree.Leaf n -> n

    let private rootBoundingBox (h : PatchHierarchy) : Box3d =
        (rootPatch h).info.GlobalBoundingBox


    let actualTextureLayerCount (h : PatchHierarchy) : int =
        let textures = (rootPatch h).info.Textures
        List.length textures / 2

    /// Number of distinct texture layers on the root patch. The on-disk
    /// `Textures` list contains a (texture, weights) pair per layer, so the
    /// effective layer count is `length / 2` — this matches the modulo the
    /// geospatial loader uses internally for `LegacyId` lookups.
    let textureLayerCount (h : PatchHierarchy) : int =
            max 1 (actualTextureLayerCount h)

    let hasMoreThanOneTextureLayer (h : PatchHierarchy) : bool =
        actualTextureLayerCount h > 1


    let logTextureLayersForPath (path : string) (h : PatchHierarchy) =
        let textures = (rootPatch h).info.Textures

        Log.line "[OpcLoading] %s has %d Textures entries = %d texture layers:"
            path
            (List.length textures)
            (List.length textures / 2)

        textures
        |> List.iteri (fun i t ->
            Log.line "  [%d] %s" i t.fileName
        )

    let private commonTextureCount (loaded : list<PatchHierarchy * string * int>) : int =
        let counts =
            loaded
            |> List.map (fun (_, _, count) -> count)
            |> List.distinct

        match counts with
        | [] ->
            0

        | [count] ->
            count

        | _ ->
            let count = counts |> List.min
            Log.warn
                "[OpcLoading] Loaded hierarchies have different texture counts %A; using minimum %d"
                counts
                count
            count

    let loadHierarchiesWithMoreThanOneTexture
            (basePaths : list<string>)
            : list<PatchHierarchy * string> * Box3d * int =

        let loaded =
            basePaths
            |> List.choose (fun bp ->
                try
                    let h =
                        PatchHierarchy.load
                            serializer.Pickle
                            serializer.UnPickle
                            (OpcPaths.OpcPaths bp)

                    let layerCount = actualTextureLayerCount h

                    if layerCount > 1 then
                        Log.line
                            "[OpcLoading] Loaded hierarchy with %d texture layers: %s"
                            layerCount
                            bp

                        Some (h, bp, layerCount)
                    else
                        Log.line
                            "[OpcLoading] Skipped hierarchy with only %d texture layer: %s"
                            layerCount
                            bp

                        None

                with ex ->
                    Log.warn
                        "[OpcLoading] Failed to inspect/load hierarchy %s: %s"
                        bp
                        ex.Message

                    None
            )
        
        let hierarchies =
            loaded
            |> List.map (fun (h, bp, _) -> h, bp)

        let combined =
            if List.isEmpty hierarchies then
                Box3d.Invalid
            else
                hierarchies
                |> Seq.map (fst >> rootBoundingBox)
                |> Box3d

        let textureCount =
            commonTextureCount loaded

        hierarchies, combined, textureCount

    /// For diagnostics: print the texture-list ordering so callers can map
    /// `LegacyId i` to the actual filename.
    let logTextureLayers (h : PatchHierarchy) =
        let textures = (rootPatch h).info.Textures
        Log.line "[OpcLoading] root patch has %d Textures entries:" (List.length textures)
        textures
        |> List.iteri (fun i t ->
            Log.line "  [%d] %s" i t.fileName)

    /// Loads each patch hierarchy from disk and combines the root-node
    /// bounding boxes (i.e. the *lowest-quality* coverage of every OPC).
    /// Returns (loadedHierarchies, combinedBox).
    let loadHierarchies (basePaths : list<string>) : list<PatchHierarchy * string> * Box3d =
        let hierarchies =
            basePaths
            |> List.map (fun bp ->
                let hierarchyFile = Path.Combine(bp, "patches", "patchhierarchy.xml")

                let h =
                    PatchHierarchy.load
                        serializer.Pickle
                        serializer.UnPickle
                        (OpcPaths.OpcPaths bp)

                Log.line "[OpcLoading] Successfully loaded hierarchy folder: %s" bp

                h, bp)

        let combined =
            hierarchies
            |> Seq.map (fst >> rootBoundingBox)
            |> Box3d
        hierarchies, combined

    /// Default texture id used by the multi-texturing pipeline when nothing
    /// more specific is selected. `LegacyId 0` picks the first texture layer.
    //let private defaultSecondaryTextureId : TextureId =
    //    { texture = TextureReference.LegacyId 0
    //      channel = ChannelReference.ChannelWithIndex 0 }

    /// Build the scene graph for one patch hierarchy, including the multi-texturing
    /// hooks from `SecondaryTexture` so the UI can toggle a secondary layer on/off.
    /// The `SecondaryTextureId` attribute is applied unconditionally so that the
    /// AG-getter `SecondaryTexture.getSecondary` can resolve it; whether the
    /// secondary layer is actually shown is decided by the `UseSecondary` uniform
    /// in the fragment shader.
    let buildHierarchySg
            (signature     : IFramebufferSignature)
            (runner        : Load.Runner)
            (asyncLoading  : bool)
            (basePath      : string)
            (h             : PatchHierarchy) : ISg =
        let tree = PatchLod.toRoseTree h.tree
        let paths = OpcPaths basePath
        let loader = Aardvark.Data.PixImagePfim.Loader
        Sg.patchLodMultiTexturingWithLoader
            signature runner basePath
            DefaultMetrics.mars2
            SecondaryTexture.getSecondary
            false false
            ViewerModality.XYZ
            PatchLod.CoordinatesMapping.Local
            asyncLoading
            tree
            (Some (SecondaryTexture.textures paths))
            (Some (SecondaryTexture.vertexAttributes paths))
            loader

    /// Wraps an `ISg` in an `AttributeParameters` applicator that selects
    /// the primary texture by its `LegacyId` index. Pass `None` to fall
    /// back to the loader's default.
    let withPrimaryTextureIndex (textureIndex : aval<Option<int>>) (sg : ISg) : ISg =
        let attribs =
            textureIndex |> AVal.map (fun idx ->
                let selected =
                    idx |> Option.map (fun i ->
                        { texture = TextureReference.LegacyId i
                          channel = ChannelReference.ChannelWithIndex 0 })
                { AttributeParameters.defaultParams with selectedTexture = selected })
        Sg.AttributeParameters attribs sg

    let withSecondaryTextureIndex (textureIndex : aval<Option<int>>) (sg : ISg) : ISg =
        let textureId =
            textureIndex |> AVal.map (fun idx ->
                idx |> Option.map (fun i ->
                    { texture = TextureReference.LegacyId i
                      channel = ChannelReference.ChannelWithIndex 0 }))
        sg |> SecondaryTexture.Sg.applySecondaryTextureId textureId

