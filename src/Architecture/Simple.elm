module Architecture.Simple (..) where

{-| This module is used for creating a simpler Signal that does not need to
rely on Effects. In other respects, it is the same as the base Architecture
module.

It is designed to work perfectly with [the Elm Architecture][arch] which
describes a simple architecture pattern that makes testing and refactoring
shockingly pleasant. Definitely read [the tutorial][arch] to get started!

[arch]: https://github.com/evancz/elm-architecture-tutorial/

# Start your Application
@docs start, Config
-}

import Debug
import Signal exposing (Address)
import Effects
import Architecture


{-| The configuration has three key components:

  * `model` &mdash; a big chunk of data fully describing your application.

  * `view` &mdash; a way to convert your model into your desired output type.
    It takes in two arguments. One is the model, which contains *all* the
    information about our app. The other is an [`Address`][address] that helps
    us handle user input. Whenever there is a click or key press, we send a
    message to the address describing what happened and where.

  * `update` &mdash; a function to update your model. Whenever an event
    occurs, is routed through the `Address` to this update function. We take
    in the message and the current model, then we give back a new model!

[The Elm Architecture][arch] augments this basic pattern to give you all the
modularity you want. But since we have whole model in one place, it is
also really easy to support features like *save* and *undo* that can be quite
hard in other languages.

[address]: http://package.elm-lang.org/packages/elm-lang/core/2.0.1/Signal#Mailbox
[arch]: https://github.com/evancz/elm-architecture-tutorial/
-}
type alias Config model action output =
  { model : model
  , view : Address action -> model -> output
  , update : action -> model -> model
  }


{-| This starts up your application. The following code sets up a counter
that can be incremented and decremented. You can read more about writing
programs like this [here](https://github.com/evancz/elm-architecture-tutorial/).

    import Graphics.Element exposing (show, flow, down)
    import Graphics.Input exposing (button)
    import Architecture.Simple as Architecture

    main =
      Architecture.start { model = model, view = view, update = update }

    model = 0

    view address model =
      flow down <|
        [ button (Signal.message address Decrement) "-"
        , show model
        , button (Signal.message address Increment) "+"
        ]

    type Action = Increment | Decrement

    update action model =
      case action of
        Increment -> model + 1
        Decrement -> model - 1

Notice that the program cleanly breaks up into model, update, and view.
This means it is super easy to test your update logic independent of any
rendering.
-}
start : Config model action output -> Signal output
start config =
  let
    generalConfig =
      { init = ( config.model, Effects.none )
      , update = (\action model -> ( config.update action model, Effects.none ))
      , view = config.view
      , inputs = []
      }

    app =
      Architecture.start generalConfig
  in
    app.output
