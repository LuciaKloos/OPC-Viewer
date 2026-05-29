module PRo3D.Viewer.SimpleGui.Styles

let renderArea =
    "position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: 0"

let overlayToolbar =
    "width: 15rem; position: fixed; top: 8px; left: 8px; z-index: 10; \
        padding: 8px 10px; background: rgba(20,20,20,0.75); color: #eee; \
        font-family: sans-serif; border-radius: 4px"

let overlayLegend =
    "position: fixed; left: 50%; bottom: 72px; transform: translateX(-50%); z-index: 9; \
        width: min(55vw, 460px); pointer-events: none; font-family: sans-serif; \
        background: rgba(20,20,20,0.75); color: #eee; \
        text-shadow: 0 1px 2px rgba(0,0,0,0.8); border-radius: 4px; \
        padding: 6px 8px; box-sizing: border-box"

let overlayColorMapLabel =
    "font-family: sans-serif; width: 100%; display: flex; justify-content: space-between; font-size: 12px; margin-top: 4px; "

let wavelengthRangeSection =
    "margin-top: 8px"

let wavelengthRangeTitle =
    "font-size: 12px; margin-bottom: 2px"

let wavelengthRangeLimitLabel =
    "display: flex; justify-content: space-between; font-size: 11px; opacity: 0.8; margin-bottom: 4px"

let wavelengthRangeInputRow =
    "display: flex; justify-content: space-between; font-size: 11px; align-items: center"

let wavelengthRangeInput =
    "width: 70px"

let wavelengthRangeUnit =
    "font-size: 11px; opacity: 0.85"

let wavelengthRangeWarning =
    "font-size: 12px; margin-top: 4px; color: #ffaaaa"

let wavelengthRangeMuted =
    "font-size: 12px; margin-top: 4px; opacity: 0.75"
