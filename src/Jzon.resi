module DecodingError: {
  type locationComponent = Field(string) | Index(int)

  type location = array<locationComponent>

  type t = [
    | #SyntaxError(string)
    | #MissingField(location, string)
    | #UnexpectedJsonType(location, string, Js.Json.t)
    | #UnexpectedJsonValue(location, string)
  ]

  let appendLocation: (t, locationComponent) => t
  let prependLocation: (t, locationComponent) => t
  let toString: t => string
}

type codec<'v>
type field<'v>

let decode: (codec<'v>, Js.Json.t) => result<'v, DecodingError.t>
let decodeString: (codec<'v>, string) => result<'v, DecodingError.t>

let encode: (codec<'v>, 'v) => Js.Json.t
let encodeString: (codec<'v>, 'v) => string

let custom: ('v => Js.Json.t, Js.Json.t => result<'v, DecodingError.t>) => codec<'v>

let string: codec<string>
let float: codec<float>
let int: codec<int>
let bool: codec<bool>
let json: codec<Js.Json.t>

let nullable: codec<'v> => codec<option<'v>>
let nullAs: (codec<'v>, 'v) => codec<'v>
let array: codec<'v> => codec<array<'v>>
let dict: codec<'v> => codec<Js.Dict.t<'v>>

let field: (string, codec<'v>) => field<'v>
let self: field<Js.Json.t>
let optional: field<'v> => field<option<'v>>
let default: (field<'v>, 'v) => field<'v>

let asObject: Js.Json.t => result<Js.Dict.t<Js.Json.t>, DecodingError.t>

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

let object4: (
  'r => ('f1, 'f2, 'f3, 'f4),
  (('f1, 'f2, 'f3, 'f4)) => result<'r, DecodingError.t>,
  field<'f1>,
  field<'f2>,
  field<'f3>,
  field<'f4>,
) => codec<'r>

let object5: (
  'r => ('f1, 'f2, 'f3, 'f4, 'f5),
  (('f1, 'f2, 'f3, 'f4, 'f5)) => result<'r, DecodingError.t>,
  field<'f1>,
  field<'f2>,
  field<'f3>,
  field<'f4>,
  field<'f5>,
) => codec<'r>

let object6: (
  'r => ('f1, 'f2, 'f3, 'f4, 'f5, 'f6),
  (('f1, 'f2, 'f3, 'f4, 'f5, 'f6)) => result<'r, DecodingError.t>,
  field<'f1>,
  field<'f2>,
  field<'f3>,
  field<'f4>,
  field<'f5>,
  field<'f6>,
) => codec<'r>

let object7: (
  'r => ('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7),
  (('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7)) => result<'r, DecodingError.t>,
  field<'f1>,
  field<'f2>,
  field<'f3>,
  field<'f4>,
  field<'f5>,
  field<'f6>,
  field<'f7>,
) => codec<'r>

let object8: (
  'r => ('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8),
  (('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8)) => result<'r, DecodingError.t>,
  field<'f1>,
  field<'f2>,
  field<'f3>,
  field<'f4>,
  field<'f5>,
  field<'f6>,
  field<'f7>,
  field<'f8>,
) => codec<'r>

let object9: (
  'r => ('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9),
  (('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9)) => result<'r, DecodingError.t>,
  field<'f1>,
  field<'f2>,
  field<'f3>,
  field<'f4>,
  field<'f5>,
  field<'f6>,
  field<'f7>,
  field<'f8>,
  field<'f9>,
) => codec<'r>

let object10: (
  'r => ('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10),
  (('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10)) => result<'r, DecodingError.t>,
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
) => codec<'r>

let object11: (
  'r => ('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10, 'f11),
  (('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10, 'f11)) => result<'r, DecodingError.t>,
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
) => codec<'r>

let object12: (
  'r => ('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10, 'f11, 'f12),
  (('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10, 'f11, 'f12)) => result<'r, DecodingError.t>,
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
) => codec<'r>

let object13: (
  'r => ('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10, 'f11, 'f12, 'f13),
  (('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10, 'f11, 'f12, 'f13)) => result<
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
) => codec<'r>

let object14: (
  'r => ('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10, 'f11, 'f12, 'f13, 'f14),
  (('f1, 'f2, 'f3, 'f4, 'f5, 'f6, 'f7, 'f8, 'f9, 'f10, 'f11, 'f12, 'f13, 'f14)) => result<
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
