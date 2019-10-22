module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Thumbor
import Thumbor.Filter


suite : Test
suite =
    describe "elm-thumbor"
        [ describe "Thumbor.url"
            [ test "creates valid Thumbor URL, regardless of attribute order" <|
                \_ ->
                    let
                        first =
                            thumbor [ Thumbor.sizeFixed 100 200, Thumbor.fitIn Thumbor.NormalFitIn ] testImageUrl

                        second =
                            thumbor [ Thumbor.fitIn Thumbor.NormalFitIn, Thumbor.sizeFixed 100 200 ] testImageUrl
                    in
                    Expect.equal first second
            , test "ignores previous attributes of the same kind" <|
                \_ ->
                    let
                        first =
                            thumbor [ Thumbor.sizeFixed 100 200, Thumbor.sizeFixed 200 400, Thumbor.sizeFixed 400 800 ] testImageUrl

                        second =
                            thumbor [ Thumbor.sizeFixed 400 800 ] testImageUrl
                    in
                    Expect.equal first second
            , test "allows unsafe URLs" <|
                \_ ->
                    thumborUnsafe [ Thumbor.sizeFixed 100 100 ] testImageUrl
                        |> String.startsWith (thumborBaseUrl ++ "/unsafe")
                        |> Expect.true "expect path to start with unsafe"
            ]
        ]


thumborBaseUrl : String
thumborBaseUrl =
    "http://example.com/thumbor"


thumborSecret : String
thumborSecret =
    "secret"


thumbor : List Thumbor.Attribute -> String -> String
thumbor =
    Thumbor.url { baseUrl = thumborBaseUrl, key = Just thumborSecret }


thumborUnsafe : List Thumbor.Attribute -> String -> String
thumborUnsafe =
    Thumbor.url { baseUrl = thumborBaseUrl, key = Nothing }


testImageUrl : String
testImageUrl =
    "https://example.com/image.png"
