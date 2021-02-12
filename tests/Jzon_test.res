open Test

module Assert = {
  let okOf = (~message=?, left, right) =>
    assertion(
      (left, right) => left == Ok(right),
      left,
      right,
      ~operator="left == Ok(right)",
      ~message?,
    )

  let roundtrips = (~message=?, data, codec) => {
    let json = codec->Jzon.encode(data)
    let result = codec->Jzon.decode(json)
    result->okOf(data, ~message?)
  }

  let errorString = (~message=?, left, right) => assertion((left, right) => {
      switch left {
      | Ok(_) => false
      | Error(err) => err->Jzon.DecodingError.toString == right
      }
    }, left, right, ~operator="error string of left == right", ~message?)
}

type shape =
  | Circle(float)
  | Rectangle(float, float)
  | Ellipse(float, float)

type look = {
  color: string,
  size: float,
}

type vertex = {
  x: float,
  y: float,
  look: look,
}

module JsonCodecs = {
  let radius = Jzon.object1(r => r, r => r->Ok, Jzon.field("r", Jzon.float))

  let widthHeight = Jzon.object2(
    ((w, h)) => (w, h),
    ((w, h)) => (w, h)->Ok,
    Jzon.field("width", Jzon.float),
    Jzon.field("height", Jzon.float),
  )

  let shape = Jzon.object2(
    shape =>
      switch shape {
      | Circle(r) => ("circle", radius->Jzon.encode(r))
      | Rectangle(width, height) => ("rectangle", widthHeight->Jzon.encode((width, height)))
      | Ellipse(width, height) => ("ellipse", widthHeight->Jzon.encode((width, height)))
      },
    ((kind, json)) =>
      switch kind {
      | "circle" => radius->Jzon.decode(json)->Result.map(r => Circle(r))
      | "rectangle" => widthHeight->Jzon.decode(json)->Result.map(((w, h)) => Rectangle(w, h))
      | "ellipse" => widthHeight->Jzon.decode(json)->Result.map(((w, h)) => Ellipse(w, h))
      | x => Error(#UnexpectedJsonValue([Field("kind")], x))
      },
    Jzon.field("kind", Jzon.string),
    Jzon.self,
  )

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

test("Scalar types", () => {
  Assert.roundtrips("Hello", Jzon.string, ~message="string does roundtrip")
  Assert.roundtrips(42.5, Jzon.float, ~message="float does roundtrip")
  Assert.roundtrips(42, Jzon.int, ~message="int does roundtrip")
  Assert.roundtrips(true, Jzon.bool, ~message="bool does roundtrip")
  Assert.roundtrips(Js.Json.number(42.), Jzon.json, ~message="Raw JSON does roundtrip")
})

test("Int codec", () => {
  Jzon.int
  ->Jzon.decodeString("42.5")
  ->Assert.errorString("Unexpected value 42.5 at .", ~message="barks on fractional numbers")

  Jzon.int
  ->Jzon.decodeString("9111222333")
  ->Assert.errorString("Unexpected value 9111222333 at .", ~message="barks on out-of-range numbers")
})

test("Vertex decode (nested record)", () => {
  let json = `{"x": 10, "y": 20, "look": {"color": "#09a", "size": 5.0}}`
  let result = JsonCodecs.vertex->Jzon.decodeString(json)
  result->Assert.okOf(
    {x: 10.0, y: 20.0, look: {color: "#09a", size: 5.0}},
    ~message="decodes correctly",
  )
})

test("Vertex roundtrip (nested record)", () => {
  let data = {x: 10.0, y: 20.0, look: {color: "#09a", size: 5.0}}
  let json = JsonCodecs.vertex->Jzon.encode(data)
  let result = JsonCodecs.vertex->Jzon.decode(json)
  result->Assert.okOf(data, ~message="preserves data")
})

test("Shape decode (tagged union)", () => {
  let json = `{
    "kind": "rectangle",
    "width": 3,
    "height": 4
  }`

  let result = JsonCodecs.shape->Jzon.decodeString(json)
  result->Assert.okOf(Rectangle(3.0, 4.0), ~message="decodes correctly")
})

test("Shape roundtrip (tagged union)", () => {
  let data = Rectangle(3.0, 4.0)
  let json = JsonCodecs.shape->Jzon.encode(data)
  let result = JsonCodecs.shape->Jzon.decode(json)
  result->Assert.okOf(data, ~message="preserves data")
})

test("JSON with syntax error", () => {
  // Quotes around `size` are missing
  let json = `{"color": "#09a", size: 5.0}`
  let result = JsonCodecs.look->Jzon.decodeString(json)
  result->Assert.errorString(
    "Unexpected token s in JSON at position 18",
    ~message="returns Result.Error",
  )
})

test("JSON with missing field", () => {
  // `size` is missing
  let json = `{"color": "#09a"}`
  let result = JsonCodecs.look->Jzon.decodeString(json)
  result->Assert.errorString(`Missing field "size" at .`, ~message="returns #MissingField error")
})

test("JSON with missing nested field", () => {
  // `look.size` is missing
  let json = `{"x": 10, "y": 20, "look": {"color": "#09a"}}`
  let result = JsonCodecs.vertex->Jzon.decodeString(json)
  result->Assert.errorString(
    `Missing field "size" at ."look"`,
    ~message="returns #MissingField error with proper path",
  )
})

test("JSON with unexpected type", () => {
  // `size` should be a number
  let json = `{"x": 10, "y": 20, "look": {"color": "#09a", "size": "laaaarge"}}`
  let result = JsonCodecs.vertex->Jzon.decodeString(json)
  result->Assert.errorString(
    `Expected number, got string at ."look"."size"`,
    ~message="returns #UnexpectedJsonType error",
  )
})
