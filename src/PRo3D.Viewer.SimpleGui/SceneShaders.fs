module PRo3D.Viewer.SimpleGui.SceneShaders

open Aardvark.Base
open Aardvark.Rendering

open Aardvark.Rendering.Effects
open FShade

type UniformScope with
    member x.UseSecondary : bool = uniform?UseSecondary
    member x.SecondaryOpacity : float32 = uniform?SecondaryOpacity
    member x.MousePos : V2f = uniform?MousePos
    member x.ViewportSize : V2f = uniform?ViewportSize
    member x.LensRadius : float32 = uniform?LensRadius
    member x.LensAsRectangle : bool = uniform?LensAsRectangle
    member x.TextureCombiner : int = uniform?TextureCombiner
    member x.TransferFunctionMode : int = uniform?TransferFunctionMode
    member x.TFRange : V2f = uniform?TFRange
    member x.TFBlendFactor : float32 = uniform?TFBlendFactor

let private secondarySampler =
    sampler2d {
        texture uniform?SecondaryTexture
        filter Filter.MinMagMipLinear
        addressU WrapMode.Wrap
        addressV WrapMode.Wrap
    }

let private transferFunctionSampler =
    sampler2d {
        texture uniform?SecondaryTextureTransferFunction
        filter Filter.MinMagPoint
        addressU WrapMode.Clamp
        addressV WrapMode.Clamp
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
        // check if pixel is inside lens area:
        // screen-space fragment position in pixels
        let clip = v.pos
        let ndc = clip.XY / clip.W

        let fragPx = 
            V2f (
                (ndc.X * 0.5f + 0.5f) * uniform.ViewportSize.X,
                (ndc.Y * 0.5f + 0.5f) * uniform.ViewportSize.Y
            )

        // mouse position in same coordinate system as fragPos
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
                
        let range = uniform.TFRange
            
        let baseColor = v.c
        let secondaryColor = secondarySampler.Sample(v.tc)
           

        let secondaryColorTF = 
            match uniform.TransferFunctionMode with
            | 0 ->  // ramp
                let range = uniform.TFRange

                if secondaryColor.X > range.X && secondaryColor.X < range.Y then
                    let my = (secondaryColor.X - range.X) / (range.Y - range.X)
                    transferFunctionSampler.Sample(V2f(my, 0.5f))
                else
                    V4f(1.0f, 0.0f, 0.0f, 1.0f) // debug : red outside of range
                        
            | 1 ->  // passthrough
                secondaryColor
            | _ ->
                baseColor

        // first compute secondary texture without lens
        let combinedColor : V4f =
            if uniform.UseSecondary then
                match uniform.TextureCombiner with
                | 0 ->  // none
                    secondaryColorTF

                | 1 ->  // multiply
                    V4f(baseColor.XYZ * secondaryColorTF.XYZ, 2.0f)

                | 2 ->  // blend
                    V4f(baseColor.XYZ * (1.0f - uniform.TFBlendFactor) + 
                        secondaryColorTF.XYZ * uniform.TFBlendFactor, 
                        1.0f)

                | _ -> 
                        baseColor
            else
                baseColor

        let secondaryMix =
            if uniform.UseSecondary && insideLens then  
                uniform.SecondaryOpacity
            else
                0.0f

        let rgb =
            Fun.Lerp(secondaryMix, baseColor.XYZ, combinedColor.XYZ)

        let alpha = 
            if uniform.UseSecondary && insideLens then
                1.0f
            else
                baseColor.W
                       
        let debugRed = 
            if insideLens then
                combinedColor.XYZ
            else
                V3f(1.0f, 1.0f, 1.0f)

        return V4f(rgb, alpha)
    }
