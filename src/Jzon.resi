module DecodingError: {
  type locationComponent = Field(string)

  type location = array<locationComponent>

  type t = [
    | #SyntaxError(string)
    | #MissingField(location, string)
    | #UnexpectedJsonType(location, string, Js.Json.t)
  ]

  let toString: t => string
}

module Codec: {
  type t<'v>
  type encode<'v> = 'v => Js.Json.t
  type decode<'v> = Js.Json.t => result<'v, DecodingError.t>

  let make: (encode<'v>, decode<'v>) => t<'v>
}

module Field: {
  type t<'v>

  let key: t<'v> => string
  let codec: t<'v> => Codec.t<'v>
}

let string: Codec.t<string>
let float: Codec.t<float>

let field: (string, Codec.t<'v>) => Field.t<'v>
let record2: ((('a, 'b)) => 'r, 'r => ('a, 'b), Field.t<'a>, Field.t<'b>) => Codec.t<'r>
let record3: (
  (('a, 'b, 'c)) => 'r,
  'r => ('a, 'b, 'c),
  Field.t<'a>,
  Field.t<'b>,
  Field.t<'c>,
) => Codec.t<'r>

let decodeString: (string, Codec.t<'v>) => result<'v, DecodingError.t>
