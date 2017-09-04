module Profile exposing (Profile, decodeProfile, apply)

{-| This module provides a Profile JSON decoder and the ability to use the data wrapped inside the Profile.
-}

import Json.Decode as Decode
import Json.Decode.Pipeline as JDP exposing (decode, required)


{-| To the outside world, this is a Profile. It can only be created within this module, so users of Profile can rest assured it's legit! To access it's juicy contents, use the apply function.
-}
type Profile
    = Profile ProfileData


{-| The actual Profile.
-}
type alias ProfileData =
    { username : String }


{-| Profile JSON decoder.
-}
decodeProfile : Decode.Decoder Profile
decodeProfile =
    Decode.at [ "user" ] decodeMore


decodeMore : Decode.Decoder Profile
decodeMore =
    decode ProfileData
        |> JDP.required "username" Decode.string
        |> Decode.map Profile


{-| Takes a function you provide and applies it to the Profile's data. Have fun! Whatever you return in your function is yours to keep.
-}
apply : (ProfileData -> a) -> Profile -> a
apply f (Profile data) =
    f data
