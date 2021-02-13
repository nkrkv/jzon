# Jzon How-To Guides

This document list common JSON coding tasks and how they could be solved with Jzon.

## How to encode/decode a record

```rescript
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
    Jzon.field("color", Jzon.string),
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

  // Missing some required fields
  Codecs.point->Jzon.decodeString(`{"x":1,"y":2}`)
  ->Assert.equals(Error(#MissingField([], "z")))
})
```

## How to make some fields optional

Jzon supports two flavors of the optionality: fallback to a default value and lifting to `option`. In the first case you always get your value while decoding and don’t even know whether the field was in JSON or not. And in the latter case you deal with `None` or `Some(...)` explicitly. The example below demonstrates both ways.

```rescript
type point = {
  x: float,
  y: float,
  z: float,
  color: option<string>,
}

module Codecs = {
  let point = Jzon.object4(
    ({x, y, z, color}) => (x, y, z, color),
    ((x, y, z, color)) => {x, y, z, color}->Ok,
    Jzon.field("x", Jzon.float),
    Jzon.field("y", Jzon.float),
    // Use Jzon.default adapter to provide a fallback value in case
    // the field is missing
    Jzon.field("z", Jzon.float)->Jzon.default(0.0),
    // Use Jzon.optional adapter to make the value indeed option’al
    Jzon.field("color", Jzon.string)->Jzon.optional,
  )
}

test("Optional/default encoding", () => {
  Codecs.point
  ->Jzon.encodeString({x: 1.0, y: 2.0, z: 3.0, color: Some("#fda")})
  ->Assert.equals(`{"x":1,"y":2,"z":3,"color":"#fda"}`)

  // Optional fields are omitted in output if `None` and fields
  // with default values are always encoded, even if match the
  // fallback value
  Codecs.point
  ->Jzon.encodeString({x: 1.0, y: 2.0, z: 0.0, color: None})
  ->Assert.equals(`{"x":1,"y":2,"z":0}`)
})

test("Optional/default decoding", () => {
  Codecs.point
  ->Jzon.decodeString(`{"x":1,"y":2,"z":3,"color":"#fda"}`)
  ->Assert.equals(Ok({x: 1.0, y: 2.0, z: 3.0, color: Some("#fda")}))

  Codecs.point
  ->Jzon.decodeString(`{"x":1,"y":2}`)
  ->Assert.equals(Ok({x: 1.0, y: 2.0, z: 0.0, color: None}))
})
```

## How to encode/decode an opaque type

Dealing with an opaque type is not much different from dealing with a record. You just use functions specific to the type to properly construct and destruct it.

```rescript
module Codecs = {
  let date = Jzon.object3(
    date => (
      date->Js.Date.getUTCFullYear,
      date->Js.Date.getUTCMonth +. 1.0,
      date->Js.Date.getUTCDate,
    ),
    ((year, month, day)) =>
      Js.Date.utcWithYMD(~year, ~month=month -. 1.0, ~date=day, ())
      ->Js.Date.fromFloat
      ->Ok,
    Jzon.field("year", Jzon.float),
    Jzon.field("month", Jzon.float),
    Jzon.field("day", Jzon.float),
  )
}

test("Opaque type encoding", () => {
  Codecs.date
  ->Jzon.encodeString(Js.Date.fromString("Thu, 29 Nov 1973 21:30:54.321 GMT"))
  ->Assert.equals(`{"year":1973,"month":11,"day":29}`)
})

test("Opaque type decoding", () => {
  Codecs.date
  ->Jzon.decodeString(`{"year":1973,"month":11,"day":29}`)
  ->Assert.equals(Ok(Js.Date.fromString("Thu, 29 Nov 1973 00:00:00.000 GMT")))
})
```

## How to encode/decode array of records

```rescript
type point = {
  x: float,
  y: float,
}

type plot = {
  title: string,
  points: array<point>,
}

module Codecs = {
  let point = Jzon.object2(
    ({x, y}) => (x, y),
    ((x, y)) => {x, y}->Ok,
    Jzon.field("x", Jzon.float),
    Jzon.field("y", Jzon.float),
  )

  let plot = Jzon.object2(
    ({title, points}) => (title, points),
    ((title, points)) => {title, points}->Ok,
    Jzon.field("title", Jzon.string),
    // Use the Jzon.array adapter to lift another codec to
    // a codec of an array
    Jzon.field("points", Jzon.array(point)),
  )
}

test("Array encoding", () => {
  Codecs.plot
  ->Jzon.encodeString({
    title: "My Scatter Plot",
    points: [{x: 1.0, y: 2.0}, {x: 3.0, y: 4.0}, {x: 5.0, y: 6.0}],
  })
  ->Assert.equals(
    `{"title":"My Scatter Plot","points":[{"x":1,"y":2},{"x":3,"y":4},{"x":5,"y":6}]}`
  )
})

test("Array decoding", () => {
  Codecs.plot
  ->Jzon.decodeString(`{
    "title": "My Scatter Plot",
    "points": [
      {"x":1, "y":2},
      {"x":3, "y":4},
      {"x":5, "y":6}
    ]
  }`)
  ->Assert.equals(
    Ok({
      title: "My Scatter Plot",
      points: [{x: 1.0, y: 2.0}, {x: 3.0, y: 4.0}, {x: 5.0, y: 6.0}],
    }),
  )

  // Missing field does not mean an empty array by default. However, you may use
  // the `default([])` field adaptor to express just that.
  Codecs.plot
  ->Jzon.decodeString(`{"title": "My Scatter Plot"}`)
  ->Assert.equals(Error(#MissingField([], "points")))
})
```

## How to express JSON shape dependent on a tag value

Sometimes the JSON schema is dynamic and its particular subtree depends on a value of some _tag_ field which is usually a string denoting some enum value. This scenario is perfectly OK for Jzon albeit requires some boilerplate code.

### Nested case

You can face a JSON schema where the object field shape depends on the value of another field. For example:

```js
{
  "kind": "circle",
  "geometry": {
    "r": 15
  }
}

// or

{
  "kind": "rectangle",
  "geometry": {
    "width": 10,
    "height": 20
  }
}

// or

{
  "kind": "ellipse",
  "geometry": {
    "width": 30,
    "height": 40
  }
}
```

In such case, the following codecs will do the job:

```rescript
type circle = {
  radius: float,
}

type rectangle = {
  width: float,
  height: float,
}

type ellipse = {
  rx: float,
  ry: float,
}

// The type used to express various shapes
type shape =
  | Circle(circle)
  | Rectangle(rectangle)
  | Ellipse(ellipse)

module Codecs = {
  let circle = Jzon.object1(
    ({radius}) => radius,
    radius => {radius: radius}->Ok,
    Jzon.field("radius", Jzon.float),
  )

  let rectangle = Jzon.object2(
    ({width, height}) => (width, height),
    ((width, height)) => {width, height}->Ok,
    Jzon.field("width", Jzon.float),
    Jzon.field("height", Jzon.float),
  )

  let ellipse = Jzon.object2(
    ({rx, ry}) => (rx, ry),
    ((rx, ry)) => {rx, ry}->Ok,
    Jzon.field("rx", Jzon.float),
    Jzon.field("ry", Jzon.float),
  )

  let shape = Jzon.object2(
    shape =>
      // Depending on the variant, stringify the tag for the "kind" field and
      // use appropriate codec for the geometry
      switch shape {
      | Circle(geo) => ("circle", circle->Jzon.encode(geo))
      | Rectangle(geo) => ("rectangle", rectangle->Jzon.encode(geo))
      | Ellipse(geo) => ("ellipse", ellipse->Jzon.encode(geo))
      },
    ((kind, json)) =>
      // Depending on the "kind" field value take a proper payload codec
      // and build the value in the ReScript world
      switch kind {
      | "circle" => circle->Jzon.decode(json)->Result.map(geo => Circle(geo))
      | "rectangle" => rectangle->Jzon.decode(json)->Result.map(geo => Rectangle(geo))
      | "ellipse" => ellipse->Jzon.decode(json)->Result.map(geo => Ellipse(geo))
      // Properly report bad enum value for pretty errors
      | x => Error(#UnexpectedJsonValue([Field("kind")], x))
      },
    // The tag field is just an enum string
    Jzon.field("kind", Jzon.string),
    // Pass the payload field as is for further processing
    Jzon.field("geometry", Jzon.json),
  )
}

test("Nested dependent schema encoding", () => {
  Codecs.shape
  ->Jzon.encodeString(Rectangle({width: 3.0, height: 4.0}))
  ->Assert.equals(`{"kind":"rectangle","geometry":{"width":3,"height":4}}`)

  Codecs.shape
  ->Jzon.encodeString(Circle({radius: 15.0}))
  ->Assert.equals(`{"kind":"circle","geometry":{"radius":15}}`)
})

test("Nested dependent schema decoding", () => {
  Codecs.shape
  ->Jzon.decodeString(`{"kind":"rectangle","geometry":{"width":3,"height":4}}`)
  ->Assert.equals(Ok(Rectangle({width: 3.0, height: 4.0})))

  Codecs.shape
  ->Jzon.decodeString(`{"kind":"circle","geometry":{"radius":15}}`)
  ->Assert.equals(Ok(Circle({radius: 15.0})))

  Codecs.shape
  ->Jzon.decodeString(`{"kind":"donut","geometry":{"radius":15}}`)
  ->Assert.equals(Error(#UnexpectedJsonValue([Field("kind")], "donut")))
})
```

### Flat case

It might happen that the dependent fields are not encapsulated in a separate tree but smashed to the same JSON object:

```js
{
  "kind": "circle",
  "r": 15
}

// or

{
  "kind": "rectangle",
  "width": 10,
  "height": 20
}

// or

{
  "kind": "ellipse",
  "width": 30,
  "height": 40
}
```

To make things worse, the ReScript representation might be arbitrary and not a record-per-variant. Nevertheless, this case is can be expressed and it’s quite similar to the nested case:

```rescript
// The type used to express various shapes
type shape =
| Circle(float)
| Rectangle(float, float)
| Ellipse(float, float)

module Codecs = {
  // Temporary codec for Circle params. Effectively a single field extractor.
  let radius = Jzon.object1(
    r => r,
    r => r->Ok,
    Jzon.field("r", Jzon.float)
  )

  // Temporary codec for Rectangle|Ellipse params. Effectively a converter
  // between JSON object and 2-tuple
  let widthHeight = Jzon.object2(
    ((w, h)) => (w, h),
    ((w, h)) => (w, h)->Ok,
    Jzon.field("width", Jzon.float),
    Jzon.field("height", Jzon.float),
  )

  let shape = Jzon.object2(
    shape =>
      // Depending on the variant, stringify the tag for the "kind" field and
      // use appropriate params codec for the rest fields
      switch shape {
      | Circle(r) => ("circle", radius->Jzon.encode(r))
      | Rectangle(width, height) => ("rectangle", widthHeight->Jzon.encode((width, height)))
      | Ellipse(width, height) => ("ellipse", widthHeight->Jzon.encode((width, height)))
      },
    ((kind, json)) =>
      // Depending on the "kind" field value take a proper params codec to decode
      // other fields and build the value in the ReScript world
      switch kind {
      | "circle" => radius->Jzon.decode(json)->Result.map(r => Circle(r))
      | "rectangle" => widthHeight->Jzon.decode(json)->Result.map(((w, h)) => Rectangle(w, h))
      | "ellipse" => widthHeight->Jzon.decode(json)->Result.map(((w, h)) => Ellipse(w, h))
      | x => Error(#UnexpectedJsonValue([Field("kind")], x))
      },
    Jzon.field("kind", Jzon.string),
    // The `self` descriptor means “this object”. It allows to further process the
    // same object with other codecs.
    Jzon.self,
  )
}

test("Flat dependent schema encoding", () => {
  Codecs.shape
  ->Jzon.encodeString(Rectangle(3.0, 4.0))
  ->Assert.equals(`{"kind":"rectangle","width":3,"height":4}`)

  Codecs.shape
  ->Jzon.encodeString(Circle(15.0))
  ->Assert.equals(`{"kind":"circle","r":15}`)
})

test("Flat dependent schema decoding", () => {
  Codecs.shape
  ->Jzon.decodeString(`{"kind":"rectangle","width":3,"height":4}`)
  ->Assert.equals(Ok(Rectangle(3.0, 4.0)))

  Codecs.shape
  ->Jzon.decodeString(`{"kind":"circle","r":15}`)
  ->Assert.equals(Ok(Circle(15.0)))

  Codecs.shape
  ->Jzon.decodeString(`{"kind":"donut","r":15}`)
  ->Assert.equals(Error(#UnexpectedJsonValue([Field("kind")], "donut")))
})
```

