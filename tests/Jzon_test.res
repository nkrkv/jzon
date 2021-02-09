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

  let errorString = (~message=?, left, right) => assertion((left, right) => {
      switch left {
      | Ok(_) => false
      | Error(err) => err->Jzon.DecodingError.toString == right
      }
    }, left, right, ~operator="error string of left == right", ~message?)
}

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
  let look = Jzon.record2(
    ((color, size)) => {color: color, size: size},
    ({color, size}) => (color, size),
    Jzon.field("color", Jzon.string),
    Jzon.field("size", Jzon.float),
  )

  let vertex = Jzon.record3(
    ((x, y, look)) => {x: x, y: y, look: look},
    ({x, y, look}) => (x, y, look),
    Jzon.field("x", Jzon.float),
    Jzon.field("y", Jzon.float),
    Jzon.field("look", look),
  )
}

test("Vertex decode (nested record)", () => {
  let json = `{"x": 10, "y": 20, "look": {"color": "#09a", "size": 5.0}}`
  let result = Jzon.decodeString(json, JsonCodecs.vertex)
  result->Assert.okOf(
    {x: 10.0, y: 20.0, look: {color: "#09a", size: 5.0}},
    ~message="decodes correctly",
  )
})

test("JSON with syntax error", () => {
  // Quotes around `size` are missing
  let json = `{"color": "#09a", size: 5.0}`
  let result = Jzon.decodeString(json, JsonCodecs.look)
  result->Assert.errorString(
    "Unexpected token s in JSON at position 18",
    ~message="returns Result.Error",
  )
})

test("JSON with missing field", () => {
  // `size` is missing
  let json = `{"color": "#09a"}`
  let result = Jzon.decodeString(json, JsonCodecs.look)
  result->Assert.errorString(`Missing field "size" at .`, ~message="returns #MissingField error")
})

test("JSON with missing nested field", () => {
  // `look.size` is missing
  let json = `{"x": 10, "y": 20, "look": {"color": "#09a"}}`
  let result = Jzon.decodeString(json, JsonCodecs.vertex)
  result->Assert.errorString(
    `Missing field "size" at ."look"`,
    ~message="returns #MissingField error with proper path",
  )
})

test("JSON with unexpected type", () => {
  // `size` should be a number
  let json = `{"x": 10, "y": 20, "look": {"color": "#09a", "size": "laaaarge"}}`
  let result = Jzon.decodeString(json, JsonCodecs.vertex)
  result->Assert.errorString(
    `Expected number, got string at ."look"."size"`,
    ~message="returns #UnexpectedJsonType error",
  )
})
