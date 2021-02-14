# Jzon

Jzon is a library for ReScript to encode and decode JSON data with type safety.

- 🎷 [Documentation](https://rescript-jzon.github.io) 🎷
- [License](./LICENSE.md): MIT

## Installation

1. `yarn add rescript-jzon`
2. Add `"rescript-jzon"` item to the `dependencies` key of `bsconfig.json`

## Quick start

Imaging you have the following ReScript types to encode and decode:

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
    ({size, color}) => (size, color),
    ((size, color)) => {size, color}->Ok,
    Jzon.field("size", Jzon.float),
    Jzon.field("color", Jzon.string),
  );

  let point = Jzon.object4(
    ({x, y, z, style}) => (x, y, z, style),
    ((x, y, z, style)) => {x, y, z, style}->Ok,
    Jzon.field("x", Jzon.float),
    Jzon.field("y", Jzon.float),
    Jzon.field("z", Jzon.float)->Jzon.default(0.0),
    Jzon.field("style", style)->Jzon.optional,
  )
}
```

Next, convert between the ReScript types and `Js.Json.t` with:

```rescript
let myJsonData =
  Codecs.point
  ->Jzon.encode({
    x: 1.0,
    y: 2.0,
    z: 3.0,
    style: Some({size: 4.0, color: "#fd0"}),
  })
```

and back with:

```rescript
let myPoint = Codecs.point->Jzon.decode(myJsonData)
```
