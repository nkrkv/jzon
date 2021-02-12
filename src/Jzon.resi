module DecodingError: {
  type locationComponent = Field(string) | Index(int)

  type location = array<locationComponent>

  type t = [
    | #SyntaxError(string)
    | #MissingField(location, string)
    | #UnexpectedJsonType(location, string, Js.Json.t)
    | #UnexpectedJsonValue(location, string)
  ]

  let toString: t => string
}

type codec<'v>
type field<'v>

let decode: (codec<'v>, Js.Json.t) => result<'v, DecodingError.t>
let decodeString: (codec<'v>, string) => result<'v, DecodingError.t>
let encode: (codec<'v>, 'v) => Js.Json.t

let string: codec<string>
let float: codec<float>
let int: codec<int>
let bool: codec<bool>
let json: codec<Js.Json.t>

let nullable: codec<'v> => codec<option<'v>>
let array: codec<'v> => codec<array<'v>>

let field: (string, codec<'v>) => field<'v>
let self: field<Js.Json.t>

let object1: ('r => 'f1, 'f1 => result<'r, DecodingError.t>, field<'f1>) => codec<'r>

let object2: (
  'r => ('f1, 'f2),
  (('f1, 'f2)) => result<'r, DecodingError.t>,
  field<'f1>,
  field<'f2>,
) => codec<'r>

let object3: (
  'r => ('f1, 'f2, 'f3),
  (('f1, 'f2, 'f3)) => result<'r, DecodingError.t>,
  field<'f1>,
  field<'f2>,
  field<'f3>,
) => codec<'r>

let object15: (
  'r => ('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10, 'f11, 'f12, 'f13, 'f14, 'f15),
  (('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10, 'f11, 'f12, 'f13, 'f14, 'f15)) => result<
    'r,
    DecodingError.t,
  >,
  field<'f1>,
  field<'f2>,
  field<'f3>,
  field<'f4>,
  field<'f5>,
  field<'f6>,
  field<'f7>,
  field<'f8>,
  field<'f9>,
  field<'f10>,
  field<'f11>,
  field<'f12>,
  field<'f13>,
  field<'f14>,
  field<'f15>,
) => codec<'r>
