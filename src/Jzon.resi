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

let record1: ('a => result<'r, DecodingError.t>, 'r => 'a, Field.t<'a>) => Codec.t<'r>
let record2: (
  (('a, 'b)) => result<'r, DecodingError.t>,
  'r => ('a, 'b),
  Field.t<'a>,
  Field.t<'b>,
) => Codec.t<'r>
let record3: (
  (('a, 'b, 'c)) => result<'r, DecodingError.t>,
  'r => ('a, 'b, 'c),
  Field.t<'a>,
  Field.t<'b>,
  Field.t<'c>,
) => Codec.t<'r>

let decodeString: (string, Codec.t<'v>) => result<'v, DecodingError.t>
