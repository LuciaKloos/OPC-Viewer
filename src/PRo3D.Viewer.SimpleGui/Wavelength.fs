module PRo3D.Viewer.SimpleGui.Wavelength

open Aardvark.Base

let tryParseFloat32Invariant (s : string) =
    let clean =
        s.Trim()
            .Trim('"')
            .Trim('\'')

    match System.Double.TryParse(
        clean,
        System.Globalization.NumberStyles.Float,
        System.Globalization.CultureInfo.InvariantCulture
    ) with
    | true, v -> Some (float32 v)
    | _ -> None

let formatFloat32Invariant (v : float32) =
    v.ToString("0.##", System.Globalization.CultureInfo.InvariantCulture)

let formatIntInvariant (v : int) =
    v.ToString(System.Globalization.CultureInfo.InvariantCulture)

let wavelengthTo01 (minNm : int) (maxNm : int) (valueNm : float32) : float32 =
        let minF = float32 minNm
        let maxF = float32 maxNm
        let span = max 1.0f (maxF - minF)

        clamp 0.0f 1.0f ((valueNm - minF) / span)

let value01ToWavelength (minNm : int) (maxNm : int) (value01 : float32) : float32 =
    let minF = float32 minNm
    let maxF = float32 maxNm
    minF + clamp 0.0f 1.0f value01 * (maxF - minF)

let fullWavelengthRange01 =
    V2f(0.0f, 1.0f)

let fullWavelengthRangeToken =
    "__FULL_WAVELENGTH_RANGE__"

let wavelengthSelectionToRangeOrFull
    (minNm : int)
    (maxNm : int)
    (selectedMinNm : float32)
    (selectedMaxNm : float32)
    : V2f =

        let minF = float32 minNm
        let maxF = float32 maxNm

        let isValid =
            selectedMinNm >= minF &&
            selectedMinNm <= maxF &&
            selectedMaxNm >= minF &&
            selectedMaxNm <= maxF &&
            selectedMinNm <= selectedMaxNm

        if isValid then
            V2f(
                wavelengthTo01 minNm maxNm selectedMinNm,
                wavelengthTo01 minNm maxNm selectedMaxNm
            )
        else
            fullWavelengthRange01

let wavelengthMinInputValueScript
    (minNm : int)
    (maxNm : int)
    (selectedMaxNm : float32)
    : string =

        sprintf
            "(function(){var raw=event.target.value.trim();var v=Number(raw);var min=%d;var max=%d;var selectedMax=%s;var invalid=(raw==='' || !Number.isFinite(v) || v < min || v > max || v > selectedMax);if(invalid){var row=event.target.closest('[data-wavelength-range-row=\"true\"]');if(row){var inputs=row.getElementsByTagName('input');if(inputs.length>=2){inputs[0].value=String(min);inputs[1].value=String(max);}}else{event.target.value=String(min);}return '%s';}return raw;})()"
            minNm
            maxNm
            (selectedMaxNm.ToString(System.Globalization.CultureInfo.InvariantCulture))
            fullWavelengthRangeToken

let wavelengthMaxInputValueScript
    (minNm : int)
    (maxNm : int)
    (selectedMinNm : float32)
    : string =

        sprintf
            "(function(){var raw=event.target.value.trim();var v=Number(raw);var min=%d;var max=%d;var selectedMin=%s;var invalid=(raw==='' || !Number.isFinite(v) || v < min || v > max || v < selectedMin);if(invalid){var row=event.target.closest('[data-wavelength-range-row=\"true\"]');if(row){var inputs=row.getElementsByTagName('input');if(inputs.length>=2){inputs[0].value=String(min);inputs[1].value=String(max);}}else{event.target.value=String(max);}return '%s';}return raw;})()"
            minNm
            maxNm
            (selectedMinNm.ToString(System.Globalization.CultureInfo.InvariantCulture))
            fullWavelengthRangeToken