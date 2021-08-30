# Jzon

Jzon is a library for ReScript to encode and decode JSON data with type safety.

- 🎷 [Documentation](https://nkrkv.github.io/jzon/) 🎷
- [Changelog](./CHANGELOG.md): MIT
- [License](./LICENSE.md): MIT

## Installation

1. `yarn add rescript-jzon`
2. Add `"rescript-jzon"` item to the `dependencies` key of `bsconfig.json`

## Quick start

Imagine you have the following ReScript types to encode and decode:

```rescript
type style = {
  size: float,
  color: string,
}

type point = {
  x: float,
  y: float,
  z: float,
  style: option<style>,
}
```

First, define their _codecs_:

```rescript
module Codecs = {
  let style = Jzon.object2(
    // Function to encode original object to linear tuple
    ({size, color}) => (size, color),

    // Function to decode linear tuple back to object
    ((size, color)) => {size, color}->Ok,

    // Field names and codecs for the tuple elements
    Jzon.field("size", Jzon.float),
    Jzon.field("color", Jzon.string),
  );

  // Similar codec for another record type
  let point = Jzon.object4(
    ({x, y, z, style}) => (x, y, z, style),
    ((x, y, z, style)) => {x, y, z, style}->Ok,
    Jzon.field("x", Jzon.float),
    Jzon.field("y", Jzon.float),
    // ... supports default values
    Jzon.field("z", Jzon.float)->Jzon.default(0.0),
    // ... may refer your other codecs
    Jzon.field("style", style)->Jzon.optional,
  )
}
```

Next, convert between the ReScript types and `Js.Json.t` with:

```rescript
let myPoint = {
  x: 1.0,
  y: 2.0,
  z: 3.0,
  style: Some({size: 4.0, color: "#fd0"}),
}

let json = myPoint->Jzon.encodeWith(Codecs.point)
```

and back with:

```rescript
let myPoint = json->Jzon.decodeWith(Codecs.point)
```
