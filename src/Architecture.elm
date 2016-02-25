module Architecture (start, Config, Module) where

{-| This module helps you create a complex signal in a typical Elm workflow.
It is an abstraction based on [StartApp][start-app]. Where StartApp allows you
to create a `Signal Html`, this package allows you to create a `Signal a`, as
long as you supply a function `view : Signal.Adress Action -> Model -> a`.
It otherwise assumes you are following [the Elm Architecture][arch] and using
[elm-effects][]. From there it will wire everything up for you!

**Be sure to [read the Elm Architecture tutorial][arch] to learn how this all
works!**

[arch]: https://github.com/evancz/elm-architecture-tutorial
[elm-effects]: http://package.elm-lang.org/packages/evancz/elm-effects/latest
[start-app]: http://package.elm-lang.org/packages/evancz/start-app/latest

# Start your Application
@docs start, Config, Module

-}

import Task
import Effects exposing (Effects, Never)


{-| The configuration of an app follows the basic model / update / view pattern
that you see in every Elm program.

The `init` transaction will give you an initial model and create any tasks that
are needed on start up.

The `update` and `view` fields describe how to step the model and view the
model. Note that the biggest difference between this package and StartApp is
that the view function can produce any type, not only Html.

The `inputs` field is for any external signals you might need. If you need to
get values from JavaScript, they will come in through a port as a signal which
you can pipe into your app as one of the `inputs`.
-}
type alias Config model action output =
  { init : ( model, Effects action )
  , update : action -> model -> ( model, Effects action )
  , view : Signal.Address action -> model -> output
  , inputs : List (Signal.Signal action)
  }


{-| An `Module` is made up of a couple signals:

  * `output` &mdash; a signal of `output` representing the current visual
    representation of your module. This is the primary Signal intended for use
    as the output of this package.

  * `model` &mdash; a signal representing the current model. Generally you
    will not need this one, but it is there just in case. You will know if you
    need this.

  * `tasks` &mdash; a signal of tasks that need to get run. Your app is going
    to be producing tasks in response to all sorts of events, so this needs to
    be hooked up to a `port` to ensure they get run.
-}
type alias Module model output =
  { output : Signal output
  , model : Signal model
  , tasks : Signal (Task.Task Never ())
  }


{-| Start an application. It requires a bit of wiring once you have created an
`Module`. It should pretty much always look like this:

    app =
        start { init = init, view = view, update = update, inputs = [] }

    output =
        app.output

    port tasks : Signal (Task.Task Never ())
    port tasks =
        app.tasks

So once we start the `Module` we feed the output signal into our main
application and feed the resulting tasks into a `port` that will run them all.
-}
start : Config model action output -> Module model output
start config =
  let
    singleton action =
      [ action ]

    -- messages : Signal.Mailbox (List action)
    messages =
      Signal.mailbox []

    -- address : Signal.Address action
    address =
      Signal.forwardTo messages.address singleton

    -- updateStep : action -> (model, Effects action) -> (model, Effects action)
    updateStep action ( oldModel, accumulatedEffects ) =
      let
        ( newModel, additionalEffects ) =
          config.update action oldModel
      in
        ( newModel, Effects.batch [ accumulatedEffects, additionalEffects ] )

    -- update : List action -> (model, Effects action) -> (model, Effects action)
    update actions ( model, _ ) =
      List.foldl updateStep ( model, Effects.none ) actions

    -- inputs : Signal (List action)
    inputs =
      Signal.mergeMany (messages.signal :: List.map (Signal.map singleton) config.inputs)

    -- effectsAndModel : Signal (model, Effects action)
    effectsAndModel =
      Signal.foldp update config.init inputs

    model =
      Signal.map fst effectsAndModel
  in
    { output = Signal.map (config.view address) model
    , model = model
    , tasks = Signal.map (Effects.toTask messages.address << snd) effectsAndModel
    }
