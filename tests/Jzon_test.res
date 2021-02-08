open Test

type look = {
  color: string,
  size: float,
}

let look = Jzon.record2(
  ((color, size)) => {color: color, size: size},
  ({color, size}) => (color, size),
  Jzon.field("color", Jzon.string),
  Jzon.field("size", Jzon.float),
)

type vertex = {
  x: float,
  y: float,
  look: look,
}

let vertex = Jzon.record3(
  ((x, y, look)) => {x: x, y: y, look: look},
  ({x, y, look}) => (x, y, look),
  Jzon.field("x", Jzon.float),
  Jzon.field("y", Jzon.float),
  Jzon.field("look", look),
)

let assertOk = (~message=?, result, payload) => assertion((result, payload) =>
    switch result {
    | Ok(payload') => payload == payload'
    | Error(_) => false
    }
  , result, payload, ~operator="isOk", ~message?)

test("Vertex decode", () => {
  let json = `{"x": 10, "y": 20, "look": {"color": "#09a", "size": 5.0}}`
  let result = Jzon.decodeString(json, vertex)
  assertOk(
    result,
    {x: 10.0, y: 20.0, look: {color: "#09a", size: 5.0}},
    ~message="decodes correctly",
  )
})
