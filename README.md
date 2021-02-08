
```
type look = {
  color: string,
  size: float,
}

type point = {
  x: float,
  y: float,
  look: look,
}

type graph = {
  name: string,
  points: array(point),
}

let decodeLook =
  Jzon.decode2(
    (color, size) => {color, size},
    Jzon.field("color")->Jzon.decodeString,
    Jzon.field("size")->Json.decodeFloat,
  );

let decodePoint =
  Jzon.decode3(
    (x, y, look) => {x, y, look},
    Jzon.field(_, "x")->Jzon.decodeFloat,
    Jzon.field(_, "y")->Jzon.decodeFloat,
    Jzon.field(_, "look")->decodeLook,
  );

let look =
  Jzon.record2(
    ((color, size)) => {color, size},
    ({color, size}) => (color, size),
    Jzon.field("color", Jzon.string),
    Jzon.field("size", Jzon.float),
  );

let point =
  Jzon.record3(
    ((x, y, look)) => {x, y, look},
    ({x, y, look}) => (x, y, look),
    Jzon.field("x", Jzon.float, ~default=42.0),
    Jzon.field("y", Jzon.float)->Jzon.optional,
    Jzon.field("look", look),
  );

let graph =
  Jzon.record2(
    ((name, points)) => {name, points},
    ({name, points}) => (name, points),
    Jzon.field("name", Jzon.string),
    Jzon.field("points", Jzon.array(point), ~alt=[|"point_list"|]),
  );


let decodeGraph =
  Jzon.decode2(
    (name, points) => {name, points},
    Jzon.field("color")->Jzon.decodeString,
    Jzon.field("points")->Jzon.fallback("point_list")->Jzon.array->decodePoints,
  );

switch (Jzon.decodeString("foobarbaz", point)) {
| Ok(_) =>
| Error(err) =>
}
```
