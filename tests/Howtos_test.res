open Test

module Quickstart = {
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

  module Codecs = {
    let style = Jzon.object2(
      ({size, color}) => (size, color),
      ((size, color)) => {size: size, color: color}->Ok,
      Jzon.field("size", Jzon.float),
      Jzon.field("color", Jzon.string),
    )

    let point = Jzon.object4(
      ({x, y, z, style}) => (x, y, z, style),
      ((x, y, z, style)) => {x: x, y: y, z: z, style: style}->Ok,
      Jzon.field("x", Jzon.float),
      Jzon.field("y", Jzon.float),
      Jzon.field("z", Jzon.float)->Jzon.default(0.0),
      Jzon.field("style", style)->Jzon.optional,
    )
  }

  let encode = () => {
    let myPoint = {x: 1.0, y: 2.0, z: 3.0, style: Some({size: 4.0, color: "#fd0"})}
    let json = myPoint->Jzon.encodeWith(Codecs.point)
    ignore(json)
  }

  let decode = myJsonData => {
    let point = Codecs.point->Jzon.decode(myJsonData)
    ignore(point)
  }
}

module HowtoRecord = {
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
      ((x, y, z, color)) => {x: x, y: y, z: z, color: color}->Ok,
      Jzon.field("x", Jzon.float),
      Jzon.field("y", Jzon.float),
      Jzon.field("z", Jzon.float),
      Jzon.field("color", Jzon.string),
    )
  }

  test("Record encoding", () => {
    {x: 1.0, y: 2.0, z: 3.0, color: "#fda"}
    ->Jzon.encodeStringWith(Codecs.point)
    ->Assert.equals(`{"x":1,"y":2,"z":3,"color":"#fda"}`)
  })

  test("Record decoding", () => {
    `{"x":1,"y":2,"z":3,"color":"#fda"}`
    ->Jzon.decodeStringWith(Codecs.point)
    ->Assert.equals(Ok({x: 1.0, y: 2.0, z: 3.0, color: "#fda"}))

    // Missing some required fields
    `{"x":1,"y":2}`
    ->Jzon.decodeStringWith(Codecs.point)
    ->Assert.equals(Error(#MissingField([], "z")))
  })
}

module HowtoOptionalDefault = {
  type point = {
    x: float,
    y: float,
    z: float,
    color: option<string>,
  }

  module Codecs = {
    let point = Jzon.object4(
      ({x, y, z, color}) => (x, y, z, color),
      ((x, y, z, color)) => {x: x, y: y, z: z, color: color}->Ok,
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
    {x: 1.0, y: 2.0, z: 3.0, color: Some("#fda")}
    ->Jzon.encodeStringWith(Codecs.point)
    ->Assert.equals(`{"x":1,"y":2,"z":3,"color":"#fda"}`)

    // Optional fields are omitted in output if `None` and fields
    // with default values are always encoded, even if match the
    // fallback value
    {x: 1.0, y: 2.0, z: 0.0, color: None}
    ->Jzon.encodeStringWith(Codecs.point)
    ->Assert.equals(`{"x":1,"y":2,"z":0}`)
  })

  test("Optional/default decoding", () => {
    `{"x":1,"y":2,"z":3,"color":"#fda"}`
    ->Jzon.decodeStringWith(Codecs.point)
    ->Assert.equals(Ok({x: 1.0, y: 2.0, z: 3.0, color: Some("#fda")}))

    `{"x":1,"y":2}`
    ->Jzon.decodeStringWith(Codecs.point)
    ->Assert.equals(Ok({x: 1.0, y: 2.0, z: 0.0, color: None}))
  })
}

module HowtoOpaque = {
  module Codecs = {
    let date = Jzon.object3(
      date => (
        date->Js.Date.getUTCFullYear,
        date->Js.Date.getUTCMonth +. 1.0,
        date->Js.Date.getUTCDate,
      ),
      ((year, month, day)) =>
        Js.Date.utcWithYMD(~year, ~month=month -. 1.0, ~date=day, ())->Js.Date.fromFloat->Ok,
      Jzon.field("year", Jzon.float),
      Jzon.field("month", Jzon.float),
      Jzon.field("day", Jzon.float),
    )
  }

  test("Opaque type encoding", () => {
    Js.Date.fromString("Thu, 29 Nov 1973 21:30:54.321 GMT")
    ->Jzon.encodeStringWith(Codecs.date)
    ->Assert.equals(`{"year":1973,"month":11,"day":29}`)
  })

  test("Opaque type decoding", () => {
    `{"year":1973,"month":11,"day":29}`
    ->Jzon.decodeStringWith(Codecs.date)
    ->Assert.equals(Ok(Js.Date.fromString("Thu, 29 Nov 1973 00:00:00.000 GMT")))
  })
}

module HowtoArrayOfRecords = {
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
      ((x, y)) => {x: x, y: y}->Ok,
      Jzon.field("x", Jzon.float),
      Jzon.field("y", Jzon.float),
    )

    let plot = Jzon.object2(
      ({title, points}) => (title, points),
      ((title, points)) => {title: title, points: points}->Ok,
      Jzon.field("title", Jzon.string),
      // Use the Jzon.array adapter to lift another codec to
      // a codec of an array
      Jzon.field("points", Jzon.array(point)),
    )
  }

  test("Array encoding", () => {
    {
      title: "My Plot",
      points: [{x: 1.0, y: 2.0}, {x: 3.0, y: 4.0}, {x: 5.0, y: 6.0}],
    }
    ->Jzon.encodeStringWith(Codecs.plot)
    ->Assert.equals(`{"title":"My Plot","points":[{"x":1,"y":2},{"x":3,"y":4},{"x":5,"y":6}]}`)
  })

  test("Array decoding", () => {
    `{
      "title": "My Plot",
      "points": [
        {"x":1, "y":2},
        {"x":3, "y":4},
        {"x":5, "y":6}
      ]
    }`
    ->Jzon.decodeStringWith(Codecs.plot)
    ->Assert.equals(
      Ok({
        title: "My Plot",
        points: [{x: 1.0, y: 2.0}, {x: 3.0, y: 4.0}, {x: 5.0, y: 6.0}],
      }),
    )

    // Missing field does not mean an empty array by default. However, you may use
    // the `default([])` field adaptor to express just that.
    `{"title": "My Plot"}`
    ->Jzon.decodeStringWith(Codecs.plot)
    ->Assert.equals(Error(#MissingField([], "points")))
  })
}

module HowtoDependentSchemaNested = {
  type circle = {radius: float}

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
      ((width, height)) => {width: width, height: height}->Ok,
      Jzon.field("width", Jzon.float),
      Jzon.field("height", Jzon.float),
    )

    let ellipse = Jzon.object2(
      ({rx, ry}) => (rx, ry),
      ((rx, ry)) => {rx: rx, ry: ry}->Ok,
      Jzon.field("rx", Jzon.float),
      Jzon.field("ry", Jzon.float),
    )

    let shape = Jzon.object2(
      shape =>
        // Depending on the variant, stringify the tag for the "kind" field and
        // use appropriate codec for the geometry
        switch shape {
        | Circle(geo) => ("circle", geo->Jzon.encodeWith(circle))
        | Rectangle(geo) => ("rectangle", geo->Jzon.encodeWith(rectangle))
        | Ellipse(geo) => ("ellipse", geo->Jzon.encodeWith(ellipse))
        },
      ((kind, json)) =>
        // Depending on the "kind" field value take a proper payload codec
        // and build the value in the ReScript world
        switch kind {
        | "circle" => json->Jzon.decodeWith(circle)->Result.map(geo => Circle(geo))
        | "rectangle" => json->Jzon.decodeWith(rectangle)->Result.map(geo => Rectangle(geo))
        | "ellipse" => json->Jzon.decodeWith(ellipse)->Result.map(geo => Ellipse(geo))
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
    Rectangle({width: 3.0, height: 4.0})
    ->Jzon.encodeStringWith(Codecs.shape)
    ->Assert.equals(`{"kind":"rectangle","geometry":{"width":3,"height":4}}`)

    Circle({radius: 15.0})
    ->Jzon.encodeStringWith(Codecs.shape)
    ->Assert.equals(`{"kind":"circle","geometry":{"radius":15}}`)
  })

  test("Nested dependent schema decoding", () => {
    `{"kind":"rectangle","geometry":{"width":3,"height":4}}`
    ->Jzon.decodeStringWith(Codecs.shape)
    ->Assert.equals(Ok(Rectangle({width: 3.0, height: 4.0})))

    `{"kind":"circle","geometry":{"radius":15}}`
    ->Jzon.decodeStringWith(Codecs.shape)
    ->Assert.equals(Ok(Circle({radius: 15.0})))

    `{"kind":"donut","geometry":{"radius":15}}`
    ->Jzon.decodeStringWith(Codecs.shape)
    ->Assert.equals(Error(#UnexpectedJsonValue([Field("kind")], "donut")))
  })
}

module HowtoDependentSchemaFlat = {
  // The type used to express various shapes
  type shape =
    | Circle(float)
    | Rectangle(float, float)
    | Ellipse(float, float)

  module Codecs = {
    // Temporary codec for Circle params. Effectively a single field extractor.
    let radius = Jzon.object1(r => r, r => r->Ok, Jzon.field("r", Jzon.float))

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
        | Circle(r) => ("circle", r->Jzon.encodeWith(radius))
        | Rectangle(width, height) => ("rectangle", (width, height)->Jzon.encodeWith(widthHeight))
        | Ellipse(width, height) => ("ellipse", (width, height)->Jzon.encodeWith(widthHeight))
        },
      ((kind, json)) =>
        // Depending on the "kind" field value take a proper params codec to decode
        // other fields and build the value in the ReScript world
        switch kind {
        | "circle" => json->Jzon.decodeWith(radius)->Result.map(r => Circle(r))
        | "rectangle" => json->Jzon.decodeWith(widthHeight)->Result.map(((w, h)) => Rectangle(w, h))
        | "ellipse" => json->Jzon.decodeWith(widthHeight)->Result.map(((w, h)) => Ellipse(w, h))
        // Properly report bad enum value for pretty errors
        | x => Error(#UnexpectedJsonValue([Field("kind")], x))
        },
      // The tag field is just an enum string
      Jzon.field("kind", Jzon.string),
      // The `self` descriptor means “this object”. It allows to further process the
      // same object with other codecs.
      Jzon.self,
    )
  }

  test("Flat dependent schema encoding", () => {
    Rectangle(3.0, 4.0)
    ->Jzon.encodeStringWith(Codecs.shape)
    ->Assert.equals(`{"kind":"rectangle","width":3,"height":4}`)

    Circle(15.0)->Jzon.encodeStringWith(Codecs.shape)->Assert.equals(`{"kind":"circle","r":15}`)
  })

  test("Flat dependent schema decoding", () => {
    `{"kind":"rectangle","width":3,"height":4}`
    ->Jzon.decodeStringWith(Codecs.shape)
    ->Assert.equals(Ok(Rectangle(3.0, 4.0)))

    `{"kind":"circle","r":15}`->Jzon.decodeStringWith(Codecs.shape)->Assert.equals(Ok(Circle(15.0)))

    `{"kind":"donut","r":15}`
    ->Jzon.decodeStringWith(Codecs.shape)
    ->Assert.equals(Error(#UnexpectedJsonValue([Field("kind")], "donut")))
  })
}
