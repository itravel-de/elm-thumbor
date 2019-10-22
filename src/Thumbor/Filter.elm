module Thumbor.Filter exposing
    ( Filter
    , backgroundColor
    , BackgroundColor(..)
    , watermark
    , WatermarkDefinition
    , WatermarkHorizontalPosition(..)
    , WatermarkVerticalPosition(..)
    , roundCorners
    , RoundCornersBackgroundColor(..)
    , format
    , ImageFormat(..)
    , fill
    , FillMode(..)
    , autoJpg
    , focal
    , blur
    , brightness
    , contrast
    , convolution
    , equalize
    , extractFocal
    , grayscale
    , maxBytes
    , noUpscale
    , noise
    , proportion
    , quality
    , rgb
    , rotate
    , sharpen
    , stretch
    , stripExif
    , stripIcc
    , upscale
    , custom
    , toString
    )

{-|

@docs Filter


# Implementations

Some filters can be quite complex and their usage might not be obvious at first sight. This package documentation will
not to try to explain what the filters do in detail, but instead link to the official Thumbor documentation.


## Background Color

@docs backgroundColor
@docs BackgroundColor


## Watermark

@docs watermark
@docs WatermarkDefinition
@docs WatermarkHorizontalPosition
@docs WatermarkVerticalPosition


## Round Corners

@docs roundCorners
@docs RoundCornersBackgroundColor


## Image Format

@docs format
@docs ImageFormat


## Fill

@docs fill
@docs FillMode


## Miscellaneous

@docs autoJpg
@docs focal
@docs blur
@docs brightness
@docs contrast
@docs convolution
@docs equalize
@docs extractFocal
@docs grayscale
@docs maxBytes
@docs noUpscale
@docs noise
@docs proportion
@docs quality
@docs rgb
@docs rotate
@docs sharpen
@docs stretch
@docs stripExif
@docs stripIcc
@docs upscale


# Advanced

@docs custom
@docs toString

-}

import Util


{-| A filter that can be used with the [filters](Thumbor#filters) attribute. See the _Implementations_ section for details and available filters.
-}
type Filter
    = Filter String (List String)


{-| Overrides the `AUTO_PNG_TO_JPG` config variable in Thumbor.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/autojpg.html>

-}
autoJpg : Bool -> Filter
autoJpg bool =
    Filter "autojpg" [ Util.boolToString bool ]


{-| Sets the background layer to the specified color. This is specifically useful when converting transparent
images (i.e. PNG) to JPEG.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/background_color.html>

-}
backgroundColor : BackgroundColor -> Filter
backgroundColor color =
    let
        colorString =
            case color of
                BackgroundColor red green blue ->
                    Util.colorByteToHexString red ++ Util.colorByteToHexString green ++ Util.colorByteToHexString blue

                AutomaticBackgroundColor ->
                    "auto"
    in
    Filter "background_color" [ colorString ]


{-| Either a specific color given in RGB (`0` to `255` for each channel) or automatic detection by Thumbor.
-}
type BackgroundColor
    = BackgroundColor Int Int Int
    | AutomaticBackgroundColor


{-| Applies a gaussian blur to the image, taking the radius as the first argument and an optional sigma as the second.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/blur.html>

-}
blur : { radius : Int, sigma : Maybe Int } -> Filter
blur { radius, sigma } =
    let
        arguments =
            [ String.fromInt radius |> Just
            , Maybe.map String.fromInt sigma
            ]
                |> List.filterMap identity
                |> String.join ","
    in
    Filter "blur" [ arguments ]


{-| Increases or decreases the image brightness in percent. Negative percentages decrease the brightness, positive ones
increase it.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/brightness.html>

-}
brightness : Int -> Filter
brightness amount =
    Filter "brightness" [ clamp -100 100 amount |> String.fromInt ]


{-| Increases or decreases the image contrast in percent. Negative percentages decrease the contrast, positive ones
increase it.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/contrast.html>

-}
contrast : Int -> Filter
contrast amount =
    Filter "contrast" [ clamp -100 100 amount |> String.fromInt ]


{-| Runs a convolution matrix (or kernel) on the image.
See [Kernel (image processing)](https://en.wikipedia.org/wiki/Kernel_%28image_processing%29) for details on the process.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/convolution.html>

-}
convolution : { matrixItems : List Int, numberOfColumns : Int, shouldNormalize : Bool } -> Filter
convolution { matrixItems, numberOfColumns, shouldNormalize } =
    let
        matrixItemsString =
            matrixItems |> List.map String.fromInt |> String.join ";"
    in
    Filter "convolution" [ matrixItemsString, String.fromInt numberOfColumns, Util.boolToString shouldNormalize ]


{-| Equalizes the color distribution in the image.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/equalize.html>

-}
equalize : Filter
equalize =
    Filter "equalize" []


{-| Tries to detect focal points within the image.
Please refer to the Thumbor documentation for more details and prerequisites.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/extract_focal_points.html>

-}
extractFocal : Filter
extractFocal =
    Filter "extract_focal" []


{-| Permits to return an image sized exactly as requested.
Empty space will be filled according to the given [FillMode](#FillMode).
Additionally, transparent areas of the source image can be filled as well.

Usually used with `NormalFitIn` or `AdaptiveFitIn` [FitInMode](Thumbor#FitInMode).

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/filling.html>

-}
fill : { mode : FillMode, fillTransparent : Bool } -> Filter
fill { mode, fillTransparent } =
    let
        fillModeString =
            case mode of
                FillModeColor red green blue ->
                    Util.colorByteToHexString red ++ Util.colorByteToHexString green ++ Util.colorByteToHexString blue

                FillModeTransparent ->
                    "transparent"

                FillModeAuto ->
                    "auto"

                FillModeBlur ->
                    "blue"
    in
    Filter "filling" [ fillModeString, Util.boolToString fillTransparent ]


{-| How to fill empty space:

  - With a specific color given in RGB (`0` to `255` for each channel)
  - With transparency, if supported by the output image format
  - With an auto-detected color, based on the image
  - With a blurred version of the image

-}
type FillMode
    = FillModeColor Int Int Int
    | FillModeTransparent
    | FillModeAuto
    | FillModeBlur


{-| This filter adds a focal point, which is used in later transforms.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/focal.html>

-}
focal : { left : Int, top : Int, right : Int, bottom : Int } -> Filter
focal { left, top, right, bottom } =
    let
        leftString =
            String.fromInt left

        topString =
            String.fromInt top

        rightString =
            String.fromInt right

        bottomString =
            String.fromInt bottom
    in
    Filter "focal" [ leftString ++ "x" ++ topString ++ ":" ++ rightString ++ "x" ++ bottomString ]


{-| Specifies the output format of the image.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/format.html>

-}
format : ImageFormat -> Filter
format imageFormat =
    let
        formatString =
            case imageFormat of
                WebP ->
                    "webp"

                Jpeg ->
                    "jpeg"

                Gif ->
                    "gif"

                Png ->
                    "png"
    in
    Filter "format" [ formatString ]


{-| -}
type ImageFormat
    = WebP
    | Jpeg
    | Gif
    | Png


{-| Changes the image to grayscale.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/grayscale.html>

-}
grayscale : Filter
grayscale =
    Filter "grayscale" []


{-| Automatically degrades the quality of the image until the image is under the specified amount of bytes.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/max_bytes.html>

-}
maxBytes : Int -> Filter
maxBytes bytes =
    Filter "max_bytes" [ bytes |> max 0 |> String.fromInt ]


{-| Tells Thumbor not to upscale your images.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/no_upscale.html>

-}
noUpscale : Filter
noUpscale =
    Filter "no_upscale" []


{-| Adds noise to the image.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/noise.html>

-}
noise : Int -> Filter
noise amount =
    Filter "noise" [ clamp 0 100 amount |> String.fromInt ]


{-| Applies proportion to height and width passed for cropping.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/proportion.html>

-}
proportion : Float -> Filter
proportion amount =
    Filter "proportion" [ clamp 0 100 amount |> String.fromFloat ]


{-| Changes the overall quality of the JPEG image (does nothing for PNGs or GIFs).

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/quality.html>

-}
quality : Int -> Filter
quality amount =
    Filter "quality" [ clamp 0 100 amount |> String.fromInt ]


{-| Changes the amount of color in each of the three channels.
The given values are percentages (`-100` to `100`).

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/rgb.html>

-}
rgb : Int -> Int -> Int -> Filter
rgb red green blue =
    Filter "rgb"
        [ clamp -100 100 red |> String.fromInt
        , clamp -100 100 green |> String.fromInt
        , clamp -100 100 blue |> String.fromInt
        ]


{-| Rotates the given image according to the angle in degrees.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/rotate.html>

-}
rotate : Int -> Filter
rotate angle =
    Filter "rotate" [ String.fromInt angle ]


{-| Adds rounded corners to the image using the specified color as background.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/round_corners.html>

-}
roundCorners : Int -> RoundCornersBackgroundColor -> Filter
roundCorners radius roundCornersBackgroundColor =
    case roundCornersBackgroundColor of
        RoundCornersBackgroundColor red green blue ->
            Filter "round_corner"
                [ String.fromInt radius
                , red |> clamp 0 255 |> String.fromInt
                , green |> clamp 0 255 |> String.fromInt
                , blue |> clamp 0 255 |> String.fromInt
                ]

        RoundCornersTransparentBackground ->
            Filter "round_corner"
                [ String.fromInt radius
                , "0"
                , "0"
                , "0"
                , "true"
                ]


{-| Either a specific color given in RGB (`0` to `255` for each channel) or transparent.
-}
type RoundCornersBackgroundColor
    = RoundCornersBackgroundColor Int Int Int
    | RoundCornersTransparentBackground


{-| Enhances apparent sharpness of the image.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/sharpen.html>

-}
sharpen : { amount : Float, radius : Float, luminanceOnly : Bool } -> Filter
sharpen { amount, radius, luminanceOnly } =
    Filter "sharpen" [ String.fromFloat amount, String.fromFloat radius, Util.boolToString luminanceOnly ]


{-| Applies resize without autocrop if fit-in option will be not passed in the arguments.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/stretch.html>

-}
stretch : Filter
stretch =
    Filter "stretch" []


{-| Removes any Exif information in the resulting image.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/strip_exif.html>

-}
stripExif : Filter
stripExif =
    Filter "strip_exif" []


{-| Removes any ICC information in the resulting image.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/strip_icc.html>

-}
stripIcc : Filter
stripIcc =
    Filter "strip_icc" []


{-| Tells thumbor to upscale your images. This only makes sense with `NormalFitIn` or `AdaptiveFitIn` [FitInMode](Thumbor#FitInMode).

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/upscale.html>

-}
upscale : Filter
upscale =
    Filter "upscale" []


{-| Adds a watermark to the image.

Tests for adding a watermark on the bottom left with a vertical and horizontal distance of 25 pixels to the border:

    Thumbor.Filter.watermark
        { imageUrl = "https://example.com/watermark.png"
        , horizontalPosition = Thumbor.Filter.LeftPixels 25
        , verticalPosition = Thumbor.Filter.BottomPixels 25
        , alphaPercentage = 50
        , widthRatio = Nothing
        , heightRatio = Nothing
        }

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/watermark.html>

-}
watermark : WatermarkDefinition -> Filter
watermark { imageUrl, horizontalPosition, verticalPosition, alphaPercentage, widthRatio, heightRatio } =
    let
        horizontalPositionString =
            case horizontalPosition of
                LeftPixels value ->
                    value |> max 0 |> String.fromInt

                LeftPercentage value ->
                    (value |> max 0 |> String.fromInt) ++ "p"

                RightPixels value ->
                    value |> max 0 |> negate |> String.fromInt

                RightPercentage value ->
                    (value |> max 0 |> negate |> String.fromInt) ++ "p"

                HorizontalCenter ->
                    "center"

                HorizontalRepeat ->
                    "repeat"

        verticalPositionString =
            case verticalPosition of
                TopPixels value ->
                    value |> max 0 |> String.fromInt

                TopPercentage value ->
                    (value |> max 0 |> String.fromInt) ++ "p"

                BottomPixels value ->
                    value |> max 0 |> negate |> String.fromInt

                BottomPercentage value ->
                    (value |> max 0 |> negate |> String.fromInt) ++ "p"

                VerticalCenter ->
                    "center"

                VerticalRepeat ->
                    "repeat"

        alphaPercentageString =
            alphaPercentage
                |> clamp 0 100
                |> String.fromInt

        widthRatioString =
            widthRatio
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "none"

        heightRatioString =
            heightRatio
                |> Maybe.map String.fromInt
                |> Maybe.withDefault "none"
    in
    Filter "watermark"
        [ imageUrl
        , horizontalPositionString
        , verticalPositionString
        , alphaPercentageString
        , widthRatioString
        , heightRatioString
        ]


{-| -}
type alias WatermarkDefinition =
    { imageUrl : String
    , horizontalPosition : WatermarkHorizontalPosition
    , verticalPosition : WatermarkVerticalPosition
    , alphaPercentage : Int
    , widthRatio : Maybe Int
    , heightRatio : Maybe Int
    }


{-| -}
type WatermarkHorizontalPosition
    = LeftPixels Int
    | LeftPercentage Int
    | RightPixels Int
    | RightPercentage Int
    | HorizontalCenter
    | HorizontalRepeat


{-| -}
type WatermarkVerticalPosition
    = TopPixels Int
    | TopPercentage Int
    | BottomPixels Int
    | BottomPercentage Int
    | VerticalCenter
    | VerticalRepeat


{-| Create a custom filter based on its name and arguments.
It is useful in case you have implemented your own Thumbor filters or to use any filters that are not supported by this package yet.

Before using this, check if there is a type safe implementation in this module and use that instead.

Tests usage:

    custom "noise" [ "40" ]

    custom "sharpen" [ "2", "1.0", "true" ]

-}
custom : String -> List String -> Filter
custom =
    Filter


{-| Transforms a `Filter` into its `String` representation for use inside a Thumbor compatible URL. There is usually
no need to use this as a consumer of this package.
-}
toString : Filter -> String
toString (Filter name arguments) =
    name ++ "(" ++ String.join "," arguments ++ ")"
