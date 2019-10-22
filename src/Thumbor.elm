module Thumbor exposing
    ( url
    , Config
    , Attribute
    , size
    , sizeFixed
    , SizeValue(..)
    , manualCrop
    , Rectangle
    , horizontalAlign
    , HorizontalAlign(..)
    , verticalAlign
    , VerticalAlign(..)
    , trim
    , trimSimple
    , TrimSource(..)
    , fitIn
    , FitInMode(..)
    , filters
    , smart
    )

{-|


# Core Functionality

@docs url


# Configuration

@docs Config


# Attributes

@docs Attribute


## Sizing

@docs size
@docs sizeFixed
@docs SizeValue


## Cropping

@docs manualCrop
@docs Rectangle
@docs horizontalAlign
@docs HorizontalAlign
@docs verticalAlign
@docs VerticalAlign


## Trimming

@docs trim
@docs trimSimple
@docs TrimSource


## Fitting

@docs fitIn
@docs FitInMode


## Miscellaneous

@docs filters
@docs smart

-}

import HmacSha1
import Thumbor.Filter exposing (Filter)
import Url.Builder
import Util


{-| Thumbor server configuration. The `baseUrl` configures the base URL your Thumbor instance is hosted on. It must contain at least an URL schema and host and can contain a custom port or path.

`key` contains your key for [URL signing](https://thumbor.readthedocs.io/en/latest/security.html#security). If `Nothing` is provided, URL signing will be disabled.

️**Important Security Notice:** Since Elm is mostly used in front-end code, **your key will be exposed in clear-text to all clients**, nullifying the effect of this security measure.

**Make sure you have an alternate concept to stop URL tampering!**

Examples:

    { baseUrl = "https://example.com"
    , key = Just "thefutureishere"
    }

    { baseUrl = "https://example.com/thumbor"
    , key = Just "thefutureishere"
    }

    { baseUrl = "https://example.com:1138/thumbor"
    , key = Just "thefutureishere"
    }

-}
type alias Config =
    { baseUrl : String
    , key : Maybe String
    }


{-| -}
type FitInMode
    = NormalFitIn
    | AdaptiveFitIn
    | FullFitIn


{-| -}
type TrimSource
    = TopLeft
    | BottomRight


{-| -}
type HorizontalAlign
    = Left
    | Center
    | Right


{-| -}
type VerticalAlign
    = Top
    | Middle
    | Bottom


{-| -}
type SizeValue
    = Fixed Int
    | Proportional
    | Original


{-| Creates a new image URL based on the given [`Config`](#Config), [`Attributes`](#Attribute) and original image URL.

Attributes can occur in any order. If an attribute is specified multiple times, the last occurrence wins - very much
like how [`elm/html`](https://package.elm-lang.org/packages/elm/html/latest/) attributes work.

You might want to use currying to ease usage of this package:

    thumbor : List Thumbor.Attribute -> String -> String
    thumbor =
        Thumbor.url
            { host = "https://example.com:1138"
            , key = Just "thefutureishere"
            }

    -- Prepare a common configuration for teaser images
    prepareTeaserImage : String -> String
    prepareTeaserImage =
        thumbor [ Thumbor.scale 200 300 ]

    -- Actually use this package to create a modified image
    view : Model -> Html msg
    view model =
        img [ src (prepareTeaserImage "https://example.com/image.jpg") ] []

-}
url : Config -> List Attribute -> String -> String
url { baseUrl, key } attributes imageUrl =
    let
        pathSegments =
            Url.Builder.relative (generatePathSegments attributes ++ [ imageUrl ]) []

        hmacSignature =
            case key of
                Just value ->
                    HmacSha1.digest value pathSegments
                        |> HmacSha1.toBase64
                        |> Result.toMaybe
                        -- This case should never happen, base64 encode itself cannot fail. We have to investigate why
                        -- the HMAC library returns a Maybe here. Fallback is an unsafe Thumbor URL.
                        |> Maybe.withDefault "unsafe"
                        |> String.replace "+" "-"
                        |> String.replace "/" "_"

                Nothing ->
                    "unsafe"
    in
    Url.Builder.crossOrigin baseUrl [ hmacSignature, pathSegments ] []


{-| Represents a Thumbor attribute, controlling how images are processed. See below for a list of available attribute constructors.
-}
type Attribute
    = Size SizeValue SizeValue
    | Filters (List Filter)
    | FitIn FitInMode
    | Trim TrimSource Int
    | ManualCrop Rectangle
    | HAlign HorizontalAlign
    | VAlign VerticalAlign
    | Smart


{-| Specifies the size of the image that will be returned by the service.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/usage.html#image-size>

-}
size : SizeValue -> SizeValue -> Attribute
size =
    Size


{-| Same as [size](#size), but always uses `Fixed` sizing for convenience.
-}
sizeFixed : Int -> Int -> Attribute
sizeFixed width height =
    Size (Fixed width) (Fixed height)


{-| Specifies that the image should **not** be auto-cropped and auto-resized to be **exactly** the specified size, and
should be fit in an imaginary box (given by the [sizing](#sizing) argument) instead.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/usage.html#fit-in>

-}
fitIn : FitInMode -> Attribute
fitIn =
    FitIn


{-| Sets a list of filters as a chain to be used. See [Thumbor.Filter](Thumbor-Filter) for available filters and their usage.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/usage.html#filters>

-}
filters : List Filter -> Attribute
filters =
    Filters


{-| Removes surrounding space in images based on its color. You can pass in the source where the color should be sampled
with a [TrimSource](#TrimSource). In addition, you can specify a color tolerance with the second parameter. The tolerance
value will be clamped between `0` and `442`.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/usage.html#trim>

-}
trim : TrimSource -> Int -> Attribute
trim =
    Trim


{-| Same as [trim](#trim), but always uses `TopLeft` source and `0` tolerance.
-}
trimSimple : Attribute
trimSimple =
    Trim TopLeft 0


{-| Useful for applications that provide custom real-time cropping capabilities.
This crop is performed before the rest of the operations, so it can be used as a prepare step _before_ resizing and smart-cropping.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/usage.html#manual-crop>

-}
manualCrop : Rectangle -> Attribute
manualCrop =
    ManualCrop


{-| A rectangle defined by its top-right and bottom-left pixel positions.
-}
type alias Rectangle =
    { topLeft : ( Int, Int )
    , bottomRight : ( Int, Int )
    }


{-| Controls where the cropping will occur if some width needs to be trimmed. The default is `Center`.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/usage.html#horizontal-align>

-}
horizontalAlign : HorizontalAlign -> Attribute
horizontalAlign =
    HAlign


{-| Controls where the cropping will occur if some height needs to be trimmed. The default is `Middle`.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/usage.html#vertical-align>

-}
verticalAlign : VerticalAlign -> Attribute
verticalAlign =
    VAlign


{-| Thumbor uses some advanced techniques for obtaining important points of the image, called focal points.

If you use this attribute, smart cropping will be performed and will override both horizontal and vertical alignments
if it finds any focal points.

Thumbor docs: <https://thumbor.readthedocs.io/en/latest/usage.html#smart-cropping>

-}
smart : Attribute
smart =
    Smart



-- Internal


generatePathSegments : List Attribute -> List String
generatePathSegments attributes =
    List.filterMap (\f -> f attributes)
        [ findTrimPathSegment
        , findManualCropPathSegment
        , findFitInSegment
        , findSizePathSegment
        , findHorizontalAlignSegment
        , findVerticalAlignSegment
        , findSmartSegment
        , findFiltersPathSegment
        ]


findHorizontalAlignSegment : List Attribute -> Maybe String
findHorizontalAlignSegment =
    List.foldl
        (\item acc ->
            case item of
                HAlign Left ->
                    Just "left"

                HAlign Right ->
                    Just "right"

                HAlign Center ->
                    Nothing

                _ ->
                    acc
        )
        Nothing


findVerticalAlignSegment : List Attribute -> Maybe String
findVerticalAlignSegment =
    List.foldl
        (\item acc ->
            case item of
                VAlign Top ->
                    Just "top"

                VAlign Bottom ->
                    Just "bottom"

                VAlign Middle ->
                    Nothing

                _ ->
                    acc
        )
        Nothing


findSmartSegment : List Attribute -> Maybe String
findSmartSegment =
    List.foldl
        (\item acc ->
            case item of
                Smart ->
                    Just "smart"

                _ ->
                    acc
        )
        Nothing


sizeValueToString : SizeValue -> String
sizeValueToString value =
    case value of
        Fixed i ->
            String.fromInt i

        Proportional ->
            "0"

        Original ->
            "orig"


findSizePathSegment : List Attribute -> Maybe String
findSizePathSegment =
    List.foldl
        (\item acc ->
            case item of
                Size width height ->
                    Just (sizeValueToString width ++ "x" ++ sizeValueToString height)

                _ ->
                    acc
        )
        Nothing


findFitInSegment : List Attribute -> Maybe String
findFitInSegment =
    List.foldl
        (\item acc ->
            case item of
                FitIn NormalFitIn ->
                    Just "fit-in"

                FitIn FullFitIn ->
                    Just "full-fit-in"

                FitIn AdaptiveFitIn ->
                    Just "adaptive-fit-in"

                _ ->
                    acc
        )
        Nothing


findFiltersPathSegment : List Attribute -> Maybe String
findFiltersPathSegment =
    List.foldl
        (\item acc ->
            case item of
                Filters filters_ ->
                    filters_
                        |> List.map Thumbor.Filter.toString
                        |> String.join ":"
                        |> (++) "filters:"
                        |> Just

                _ ->
                    acc
        )
        Nothing


findManualCropPathSegment : List Attribute -> Maybe String
findManualCropPathSegment =
    List.foldl
        (\item acc ->
            case item of
                ManualCrop { topLeft, bottomRight } ->
                    let
                        left =
                            Tuple.first topLeft |> String.fromInt

                        top =
                            Tuple.second topLeft |> String.fromInt

                        right =
                            Tuple.first bottomRight |> String.fromInt

                        bottom =
                            Tuple.second bottomRight |> String.fromInt
                    in
                    Just (left ++ "x" ++ top ++ ":" ++ right ++ "x" ++ bottom)

                _ ->
                    acc
        )
        Nothing


findTrimPathSegment : List Attribute -> Maybe String
findTrimPathSegment =
    List.foldl
        (\item acc ->
            case item of
                Trim source tolerance ->
                    let
                        normalizedTolerance =
                            clamp 0 442 tolerance

                        sourceString =
                            case source of
                                TopLeft ->
                                    "top-left"

                                BottomRight ->
                                    "bottom-right"
                    in
                    if source == BottomRight || normalizedTolerance > 0 then
                        Just ("trim:" ++ sourceString ++ ":" ++ String.fromInt normalizedTolerance)

                    else
                        Just "trim"

                _ ->
                    acc
        )
        Nothing
