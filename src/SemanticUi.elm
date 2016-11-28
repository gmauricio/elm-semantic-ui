module SemanticUi exposing
    ( render
    , container, row, col
    , button, input
    , size, fluid, placeholder, events
    , Size(..)
    )

{-| Elm bindings for Semantic UI using a declarative API and useful
abstractions.

# Rendering

@docs render

# Styling

@docs Size, size, fluid, placeholder

# Events

@docs events

# Elements

## Grid

@docs container, row, col

## Button

@docs button

## Input

@docs input

-}

import Html exposing (Html, Attribute)
import Html as H
import Html.Attributes as A


{-| An element defines a control. A control has a `state` and a function to
turn this state into an `Html Msg` object. -}
type alias Element a msg =
    { state : a
    , render : a -> Html msg
    }

{-| Create the `Html msg` for a Semantic UI `Element`.

    button "click" |> render
-}
render : Element a msg -> Html msg
render element =
    element.render element.state


update : (a -> a) -> Element a msg -> Element a msg
update f element = { element | state = f element.state }

{-| Create the `Html msg` for a grid container.

    container [html content]
-}
container : List (Html msg) -> Html msg
container = H.div [ A.class "ui grid container" ]

{-| Create the `Html msg` for a grid row.

    row [] [ div [] [] ]
-}
row : List (H.Attribute msg) -> List (Html msg) -> Html msg
row attrs = H.div ([ A.class "row" ] ++ attrs)

{-| Create the `Html msg` for a grid col.

    col "two" [ div [] [] ]
-}
col : String -> List (Html msg) -> Html msg
col n = H.div [ A.class (n ++ " wide column") ]

{-| Some elements might have different sizes. -}
type Size
    = Mini
    | Tiny
    | Small
    | Medium
    | Large
    | Big
    | Huge
    | Massive

type alias Sized a =
    { a | size : Size }

{-| Adjust the size of an element.

    button "click" |> size Huge
 -}
size : Size -> Element (Sized a) msg -> Element (Sized a) msg
size size = update <| \state -> { state | size = size }


type alias Readable a =
    { a | text : String }

type alias Writable a =
    { a | placeholder : String }

type alias WithAttributes a msg =
  { a | attrs: List (Attribute msg)}

type alias EventEmitter a msg =
  { a | events: List (Attribute msg)}

{-| Adjust the text of an element.

    button "click" |> text "DON'T click"
-}
text : String -> Element (Readable a) msg -> Element (Readable a) msg
text text = update <| \state -> { state | text = text }

{-| Set placeholder for input element

    input [] |> placeholder "write here..."
-}
placeholder : String -> Element (Writable a) msg -> Element (Writable a) msg
placeholder text = update <| \state -> { state | placeholder = text }

{-| Set placeholder for input element

    input [] |> events [ onInput InputTextMsg ]
-}
events : List (Attribute msg) -> Element (EventEmitter a msg) msg -> Element (EventEmitter a msg) msg
events events = update <| \state -> { state | events = events }

{-| Add fluid class to input element

    input [] |> fluid
-}
fluid : Element (WithAttributes a msg) msg -> Element (WithAttributes a msg) msg
fluid = update <| \state -> { state | attrs = (state.attrs ++ [ A.class "fluid" ]) }

sizeToString : Size -> String
sizeToString size =
    case size of
      Mini -> "mini"
      Tiny -> "tiny"
      Small -> "small"
      Medium -> "medium"
      Large -> "large"
      Big -> "big"
      Huge -> "huge"
      Massive -> "massive"

type Event msg = Attribute msg

type alias Button msg =
    { text : String
    , size : Size
    , events: List (Attribute msg)
    }

{-| A button with a text. -}
button : String -> Element (Button msg) msg
button text =
    let
        state =
            { text = text
            , size = Medium
            , events = []
            }

        render state =
            H.button
                ([ A.class "ui button", A.class (sizeToString state.size) ] ++ state.events)
                [ H.text state.text ]

    in Element state render


type alias Input msg =
  {
    placeholder: String
  , attrs: List (Attribute msg)
  , events: List (Attribute msg)
  }

{-| A inout with a html attributes -}
input : List (H.Attribute msg) -> Element (Input msg) msg
input attrs =
    let
      state =
        {
          placeholder = ""
        , attrs = attrs
        , events = []
        }

      render state =
        H.div ([ A.class "ui icon input" ] ++ state.attrs)
          [ H.input ([ A.placeholder state.placeholder ] ++ state.events) [] ]

    in Element state render