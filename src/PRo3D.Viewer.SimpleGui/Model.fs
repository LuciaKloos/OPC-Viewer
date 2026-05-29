namespace PRo3D.Viewer.SimpleGui

open Aardvark.Base
open Aardvark.Rendering
open Aardvark.UI.Primitives
open Adaptify

type PointOfInterestCamera = {
    Name     : string
    Position : V3d
    Forward  : V3d
    Up       : V3d
}

type WavelengthConfig = {
    Unit    : string
    Wavelengths : list<int>
}

/// State after a folder was successfully loaded:
/// the root-node bounding box and the absolute paths of every
/// patchhierarchy.xml directory (i.e. the OPCs to render).
type LoadedScene = {
    RootDirectory   : string
    HierarchyPaths  : list<string>
    BoundingBox     : Box3d
    Sky             : V3d
    /// Number of distinct primary texture layers available on the root
    /// patch. Convention: `LegacyId i` selects layer `i`; the geospatial
    /// loader takes `i mod TextureCount`. Typically the last index is the
    /// real albedo (everything before tends to be data layers — normals,
    /// gravity, lon/lat/rad, …).
    TextureCount    : int
    PointsOfInterest  : list<PointOfInterestCamera>
    WavelengthConfig : WavelengthConfig
}

// The explicit numeric values are important because the shader receives them as int uniforms.
type TextureCombiner =
    | None = 0
    | Multiply = 1
    | Blend = 2

type TransferFunctionMode =
   // | Unknown = 0
    | Ramp = 0
    | Passthrough = 1

[<ModelType>]
type Model = {
    /// Currently loaded scene. None until the user picked a folder.
    loaded            : Option<LoadedScene>
    /// Camera state (free-fly).
    cameraState       : CameraControllerState
    /// Frustum near/far derived from the bounding box.
    near              : float
    far               : float
    /// Index of the primary texture layer (`LegacyId`). Defaults to the
    /// last layer on load (= the real albedo for typical OPC datasets).
    primaryTextureIndex : int
    /// UI toggles, mirroring TestViewer.fs key bindings.
    useSecondary      : bool
    secondaryOpacity  : float32
    lodVisEnabled     : bool
    fillMode          : FillMode
    secondaryTextureIndex : int
    lensRadius      : float32
    mousePos        : V2f
    viewportSize    : V2f
    lensAsRectangle : bool
    transferFunctionMode : TransferFunctionMode
    textureCombiner : TextureCombiner
    TFBlendFactor : float32
    TFRange : V2f
    transferFunctionColorMap : string
    /// Banner / error message displayed in the UI.
    statusMessage     : string
}
