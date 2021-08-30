open Test

type shape =
  | Circle(float)
  | Rectangle(float, float)
  | Ellipse(float, float)

module Codecs = {
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
}

test("Tagged union", () => {
  `{
    "kind": "rectangle",
    "width": 3,
    "height": 4
  }`
  ->Jzon.decodeStringWith(Codecs.shape)
  ->Assert.okOf(Rectangle(3.0, 4.0), ~message="decodes correctly")

  Assert.roundtrips(Ellipse(1.0, 4.0), Codecs.shape, ~message="does roundtrip")
})
