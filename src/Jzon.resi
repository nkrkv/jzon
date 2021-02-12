module DecodingError: {
  type locationComponent = Field(string)

  type location = array<locationComponent>

  type t = [
    | #SyntaxError(string)
    | #MissingField(location, string)
    | #UnexpectedJsonType(location, string, Js.Json.t)
    | #UnexpectedJsonValue(location, string)
  ]

  let toString: t => string
}

module Codec: {
  type t<'v>
  type encode<'v> = 'v => Js.Json.t
  type decode<'v> = Js.Json.t => result<'v, DecodingError.t>

  let make: (encode<'v>, decode<'v>) => t<'v>
  let encode: (t<'v>, 'v) => Js.Json.t
  let decode: (t<'v>, Js.Json.t) => result<'v, DecodingError.t>
}

module Field: {
  type t<'v>
}

let string: Codec.t<string>
let float: Codec.t<float>

let field: (string, Codec.t<'v>) => Field.t<'v>
let self: Field.t<Js.Json.t>

let object1: ('r => 'a, 'a => result<'r, DecodingError.t>, Field.t<'a>) => Codec.t<'r>
let object2: (
  'r => ('a, 'b),
  (('a, 'b)) => result<'r, DecodingError.t>,
  Field.t<'a>,
  Field.t<'b>,
) => Codec.t<'r>
let object3: (
  'r => ('a, 'b, 'c),
  (('a, 'b, 'c)) => result<'r, DecodingError.t>,
  Field.t<'a>,
  Field.t<'b>,
  Field.t<'c>,
) => Codec.t<'r>
let object15: (
  'r => ('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10, 'f11, 'f12, 'f13, 'f14, 'f15),
  (('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10, 'f11, 'f12, 'f13, 'f14, 'f15)) => result<
    'r,
    DecodingError.t,
  >,
  Field.t<'f1>,
  Field.t<'f2>,
  Field.t<'f3>,
  Field.t<'f4>,
  Field.t<'f5>,
  Field.t<'f6>,
  Field.t<'f7>,
  Field.t<'f8>,
  Field.t<'f9>,
  Field.t<'f10>,
  Field.t<'f11>,
  Field.t<'f12>,
  Field.t<'f13>,
  Field.t<'f14>,
  Field.t<'f15>,
) => Codec.t<'r>

let decodeString: (Codec.t<'v>, string) => result<'v, DecodingError.t>
