open Test

type look = {
  color: string,
  size: float,
}

type vertex = {
  x: float,
  y: float,
  look: option<look>,
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
    Jzon.field("look", look)->Jzon.optional,
  )
}

test("Nested object", () => {
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
    {x: 10.0, y: 20.0, look: Some({color: "#09a", size: 5.0})},
    ~message="Decodes correctly",
  )

  Assert.roundtrips(
    {x: 10.0, y: 20.0, look: Some({color: "#09a", size: 5.0})},
    Codecs.vertex,
    ~message="Does roundtrip",
  )
})

test("Nested object optional field", () => {
  Codecs.vertex
  ->Jzon.decodeString(`{"x": 10, "y": 20}`)
  ->Assert.okOf({x: 10.0, y: 20.0, look: None}, ~message="Decodes to None if absent")

  Codecs.vertex
  ->Jzon.decodeString(`{"x": 10, "y": 20, "look": null}`)
  ->Assert.okOf({x: 10.0, y: 20.0, look: None}, ~message="Decodes to None if null")

  Codecs.vertex
  ->Jzon.encode({x: 10.0, y: 20.0, look: None})
  ->Js.Json.stringify
  ->Assert.equals(`{"x":10,"y":20}`, ~message="Endoding omits nulls")
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
