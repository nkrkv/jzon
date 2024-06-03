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

type link = {
  start: int,
  end: int,
  weight: float,
}

module Codecs = {
  let look = Jzon.object2(
    ({color, size}) => (color, size),
    ((color, size)) => {color, size}->Ok,
    Jzon.field("color", Jzon.string),
    Jzon.field("size", Jzon.float),
  )

  let vertex = Jzon.object3(
    ({x, y, look}) => (x, y, look),
    ((x, y, look)) => {x, y, look}->Ok,
    Jzon.field("x", Jzon.float),
    Jzon.field("y", Jzon.float),
    Jzon.field("look", look)->Jzon.optional,
  )

  let link = Jzon.object3(
    ({start, end, weight}) => (start, end, weight),
    ((start, end, weight)) => {start, end, weight}->Ok,
    Jzon.field("start", Jzon.int),
    Jzon.field("end", Jzon.int),
    Jzon.field("weight", Jzon.float)->Jzon.default(1.0),
  )
}

test("Nested object", () => {
  `{
    "x": 10,
    "y": 20,
    "look": {
      "color": "#09a",
      "size": 5.0
    }
  }`
  ->Jzon.decodeStringWith(Codecs.vertex)
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
  `{"x": 10, "y": 20}`
  ->Jzon.decodeStringWith(Codecs.vertex)
  ->Assert.okOf({x: 10.0, y: 20.0, look: None}, ~message="Decodes to None if absent")

  `{"x": 10, "y": 20, "look": null}`
  ->Jzon.decodeStringWith(Codecs.vertex)
  ->Assert.okOf({x: 10.0, y: 20.0, look: None}, ~message="Decodes to None if null")

  {x: 10.0, y: 20.0, look: None}
  ->Jzon.encodeWith(Codecs.vertex)
  ->Js.Json.stringify
  ->Assert.equals(`{"x":10,"y":20}`, ~message="Encoding omits nulls")
})

test("Object field default value", () => {
  `{"start": 0, "end": 1}`
  ->Jzon.decodeStringWith(Codecs.link)
  ->Assert.okOf({start: 0, end: 1, weight: 1.0}, ~message="Used if absent")

  `{"start": 0, "end": 1, "weight": null}`
  ->Jzon.decodeStringWith(Codecs.link)
  ->Assert.okOf({start: 0, end: 1, weight: 1.0}, ~message="Used if null")
})

test("Object JSON with missing field", () => {
  `{"color": "#09a"}`
  ->Jzon.decodeStringWith(Codecs.look)
  ->Assert.errorString(`Missing field "size" at .`, ~message="Errors")
})

test("Object JSON with missing nested field", () => {
  `{"x": 10, "y": 20, "look": {"color": "#09a"}}`
  ->Jzon.decodeStringWith(Codecs.vertex)
  ->Assert.errorString(`Missing field "size" at ."look"`, ~message="Errors with proper path")
})

test("Object JSON with unexpected type", () => {
  `{"x": 10, "y": 20, "look": {"color": "#09a", "size": "laaaarge"}}`
  ->Jzon.decodeStringWith(Codecs.vertex)
  ->Assert.errorString(`Expected number, got string at ."look"."size"`, ~message="Errors")
})
