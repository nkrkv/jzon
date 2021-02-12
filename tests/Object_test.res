open Test

type look = {
  color: string,
  size: float,
}

type vertex = {
  x: float,
  y: float,
  look: look,
}

module Codecs = {
  let look = Jzon.object2(
    ({color, size}) => (color, size),
    ((color, size)) => {color: color, size: size}->Ok,
    Jzon.field("color", Jzon.string),
    Jzon.field("size", Jzon.float),
  )

  let vertex = Jzon.object3(
    ({x, y, look}) => (x, y, look),
    ((x, y, look)) => {x: x, y: y, look: look}->Ok,
    Jzon.field("x", Jzon.float),
    Jzon.field("y", Jzon.float),
    Jzon.field("look", look),
  )
}

test("Object", () => {
  Codecs.vertex
  ->Jzon.decodeString(`{
    "x": 10,
    "y": 20,
    "look": {
      "color": "#09a",
      "size": 5.0
    }
  }`)
  ->Assert.okOf(
    {x: 10.0, y: 20.0, look: {color: "#09a", size: 5.0}},
    ~message="Nested record decodes correctly",
  )

  Assert.roundtrips(
    {x: 10.0, y: 20.0, look: {color: "#09a", size: 5.0}},
    Codecs.vertex,
    ~message="Nested record does roundtrip",
  )
})

test("Object JSON with missing field", () => {
  Codecs.look
  ->Jzon.decodeString(`{"color": "#09a"}`)
  ->Assert.errorString(`Missing field "size" at .`, ~message="Errors")
})

test("Object JSON with missing nested field", () => {
  Codecs.vertex
  ->Jzon.decodeString(`{"x": 10, "y": 20, "look": {"color": "#09a"}}`)
  ->Assert.errorString(`Missing field "size" at ."look"`, ~message="Errors with proper path")
})

test("Object JSON with unexpected type", () => {
  Codecs.vertex
  ->Jzon.decodeString(`{"x": 10, "y": 20, "look": {"color": "#09a", "size": "laaaarge"}}`)
  ->Assert.errorString(`Expected number, got string at ."look"."size"`, ~message="Errors")
})
