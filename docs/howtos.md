# Jzon How-To Guides

This document list common JSON coding tasks and how they could be solved with Jzon.

## How to encode/decode a record

```reason
// The record to encode/decode
type point = {
  x: float,
  y: float,
  z: float,
  color: string,
}

module Codecs = {
  // The codec for the record. There’s no requirement to put it
  // into a sub-module, but groupping all the codecs with the same
  // names as their respective types in a dedicated module is
  // a good idea for code organization
  let point = Jzon.object4(
    ({x, y, z, color}) => (x, y, z, color),
    ((x, y, z, color)) => {x, y, z, color}->Ok,
    Jzon.field("x", Jzon.float),
    Jzon.field("y", Jzon.float),
    Jzon.field("z", Jzon.float),
    Jzon.field("color", Jzon.string)->Jzon.default("#000"),
  )
}

test("Record encoding", () => {
  Codecs.point
  ->Jzon.encodeString({x: 1.0, y: 2.0, z: 3.0, color: "#fda"})
  ->Assert.equals(`{"x":1,"y":2,"z":3,"color":"#fda"}`)
})

test("Record decoding", () => {
  Codecs.point
  ->Jzon.decodeString(`{"x":1,"y":2,"z":3,"color":"#fda"}`)
  ->Assert.equals(Ok({x: 1.0, y: 2.0, z: 3.0, color: "#fda"}))

  // Default value for color
  Codecs.point
  ->Jzon.decodeString(`{"x":1,"y":2,"z":3}`)
  ->Assert.equals(Ok({x: 1.0, y: 2.0, z: 3.0, color: "#000"}))

  // Missing some required fields
  Codecs.point
  ->Jzon.decodeString(`{"x":1,"y":2}`)
  ->Assert.equals(Error(#MissingField([], "z")))
})
```
