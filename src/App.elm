module App
    exposing
        ( Model(..)
        , Msg
        , InitialState
        , FetchSuccessState
        , FetchingState
        , FetchCap
        , FetchErrorState
        , ProgramDefinition
        , program
        , fetch
        , process
        )

{-| This module provides the means to initialize the application and change it's state. The functions and types (model and msg) are designed such that at any point in time the application can only perform the operations it's designed to perform and nothing else. This is accomplished by:

- Representing the app's state as a sum type rather than a product type (record)
- Exposing the states for pattern matching, but requiring the constructors (tags) to take additional arguments which can only be provided via this module. As a result, the constructors cannot be applied outside of this module.

# Why use a sum type for the model (application state)?
The problem with using a record for the model, a common practice with Elm devs, is that it makes it too easy to add a field for this or that purpose without taking into account how that field needs to be maintained throughout the application's life cycle, and it also makes it easy to modify state in unintended ways. This leads to subtle defects that arise during runtime.

In contrast, by using a sum type instead you're encouraged to think of an app as a finite state machine with predetermined state transitions. When the need to track the state of this or that, you're forced to decide whether it can be bolted on to an existing state, or perhaps it needs it's own state. You also have to think about what functions are needed to transition between the states.

This may sound ardous, but if an app can be designed to only do exactly what it's designed to do, and exactly when it's supposed to do it, then there's no point in writing a test suite. In fact, unit tests would be pointless because you'd be trying to get the application to perform tasks it doesn't even have the code to perform! And not having to write and maintain unit tests saves money, and I dare say, it makes the world a better place.

# How do the states work?
The Model and the states are one in the same. It goes something like this. OK, not *something* like this... *exactly* like this:

When the application is initialized it is in the Initial state. During this state a FetchCap is available. This "capability" makes it possible for the main module to perform a profile fetch by applying the `fetch` function. In other words, it is only during states which provide FetchCap that a fetch can be performed.

Applying `fetch` produces a new state, the Fetching state. This state provides no functionality. To put it differently, there's no data available in the model. The app can only wait until the Cmd is processed and produces a Msg.

Next, either the app enters the FetchError or FetchSuccess state. FetchSuccess provides a Profile. The Profile type itself is just a wrapper which guards the profile data. Basically, if you have a Profile, you're guaranteed to have a valid one because only the Profile module can produce it. Other than the Profile, the FetchSuccess doesn't provide any functionality.

On the other hand, the FetchError state provides an Http.Error and a FetchCap; and nothing else. See sum types provide rigorous control over state! During this state the app can do something with the Http.Error, such as display it, and/or use the FetchCap to retry the fetch.
@docs Model, InitialState, FetchingState, FetchErrorState, FetchSuccessState, FetchCap
-}

import Html as Html exposing (Html)
import Http exposing (Error, get, send)
import Profile exposing (Profile, decodeProfile)


{-| This model represents the state of the application. Making the application state a sum type rather than a product type makes each state and it's requirements explicit.
-}
type Model
    = Initial InitialState FetchCap
    | Fetching FetchingState
    | FetchError FetchErrorState Error FetchCap
    | FetchSuccess FetchSuccessState Profile


{-| This type "locks down" the initial state so it cannot be set outside of this module.
-}
type InitialState
    = InitialState


{-| This type is used to "lock down" the successful fetch state so it cannot be set outside of this module.
-}
type FetchSuccessState
    = FetchSuccessState


{-| This type is used to "lock down" the fetching state so it cannot be set outside of this module.
-}
type FetchingState
    = FetchingState


{-| A pseudo-capability required to perform a Profile fetch.
-}
type FetchCap
    = FetchCap


{-| This type is used to "lock down" the fetch error state so it cannot be set outside of this module.
-}
type FetchErrorState
    = FetchErrorState


type Msg
    = RequestReceived (Result Error Profile)


{-| This record is a partial HTML program definition; it omits `init`. That's to make it impossible to accidently initialize the application twice.
-}
type alias ProgramDefinition msg =
    { view : Model -> Html msg
    , update : msg -> Model -> ( Model, Cmd msg )
    , subscriptions : Model -> Sub msg
    }


{-| Provides an initialized HTML program. Since the applications entry point, `main` returns only one of these, having `init` within makes it impossible to initialize the program twice.
-}
program : ProgramDefinition msg -> Program Never Model msg
program def =
    let
        init : ( Model, Cmd msg )
        init =
            ( Initial InitialState FetchCap, Cmd.none )
    in
        Html.program
            { init = init
            , update = def.update
            , view = def.view
            , subscriptions = def.subscriptions
            }


{-| Initiates a Profile fetch.
-}
fetch : (Msg -> msg) -> FetchCap -> ( Model, Cmd msg )
fetch f _ =
    let
        url =
            "https://www.codeschool.com/users/bijanbwb.json"

        request =
            get url decodeProfile

        cmd =
            send RequestReceived request
                |> Cmd.map f
    in
        ( Fetching FetchingState, cmd )


{-| Processes the result of a Profile fetch, transitioning to the appropriate state.
-}
process : Msg -> Model
process (RequestReceived result) =
    case result of
        Ok user ->
            FetchSuccess FetchSuccessState user

        Err err ->
            FetchError FetchErrorState err FetchCap
