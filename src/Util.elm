module Util exposing (boolToString, colorByteToHexString)

import Hex


boolToString : Bool -> String
boolToString value =
    case value of
        True ->
            "true"

        False ->
            "false"


colorByteToHexString : Int -> String
colorByteToHexString =
    clamp 0x00 0xFF >> Hex.toString
