module ResultX = {
  let mapError = (result, fn) =>
    switch result {
    | Ok(_) as ok => ok
    | Error(err) => Error(fn(err))
    }
}

module DecodingError = {
  type locationComponent = Field(string)

  type location = array<locationComponent>

  type t = [
    | #SyntaxError(string)
    | #MissingField(location, string)
    | #UnexpectedJsonType(location, string, Js.Json.t)
  ]

  let formatLocation = location =>
    "." ++
    location
    ->Array.map(s =>
      switch s {
      | Field(field) => `"` ++ field ++ `"`
      }
    )
    ->Js.Array2.joinWith(".")

  let prependLocation = (err, loc) =>
    switch err {
    | #SyntaxError(_) as err => err
    | #MissingField(location, key) =>
      let location' = [loc]->Array.concat(location)
      #MissingField(location', key)
    | #UnexpectedJsonType(location, expectation, actualJson) =>
      let location' = [loc]->Array.concat(location)
      #UnexpectedJsonType(location', expectation, actualJson)
    }

  let toString = err =>
    switch err {
    | #SyntaxError(err) => err
    | #MissingField(location, key) => `Missing field "${key}" at ${location->formatLocation}`
    | #UnexpectedJsonType(location, expectation, actualJson) =>
      let actualType = switch actualJson->Js.Json.classify {
      | JSONFalse
      | JSONTrue => "boolean"
      | JSONNull => "null"
      | JSONString(_) => "string"
      | JSONNumber(_) => "number"
      | JSONObject(_) => "object"
      | JSONArray(_) => "array"
      }

      `Expected ${expectation}, got ${actualType} at ${location->formatLocation}`
    }
}

module Codec = {
  type encode<'v> = 'v => Js.Json.t
  type decode<'v> = Js.Json.t => result<'v, DecodingError.t>
  type t<'v> = {
    encode: encode<'v>,
    decode: decode<'v>,
  }

  let make = (encode, decode) => {encode: encode, decode: decode}
  let encode = codec => codec.encode
  let decode = codec => codec.decode
}

module Field = {
  type t<'v> = {
    key: string,
    codec: Codec.t<'v>,
  }

  let make = (key, codec) => {key: key, codec: codec}
  let key = ({key}) => key
  let codec = ({codec}) => codec
}

let string = Codec.make(Js.Json.string, json =>
  switch json->Js.Json.decodeString {
  | Some(x) => Ok(x)
  | None => Error(#UnexpectedJsonType([], "string", json))
  }
)

let float = Codec.make(Js.Json.number, json =>
  switch json->Js.Json.decodeNumber {
  | Some(x) => Ok(x)
  | None => Error(#UnexpectedJsonType([], "number", json))
  }
)

let field = Field.make

let encodeField = (field, val) => (field->Field.key, field->Field.codec->Codec.encode(val))

let decodeField = (jsonObject, field) =>
  switch jsonObject->Js.Dict.get(field->Field.key) {
  | Some(childJson) =>
    field
    ->Field.codec
    ->Codec.decode(childJson)
    ->ResultX.mapError(DecodingError.prependLocation(_, Field(field->Field.key)))
  | None => Error(#MissingField([], field->Field.key))
  }

let jsonObject = keyVals => Js.Json.object_(Js.Dict.fromArray(keyVals))

let record2 = (construct, destruct, field1, field2) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2) = destruct(value)
      jsonObject([encodeField(field1, val1), encodeField(field2, val2)])
    },
    // decode
    json => {
      switch json->Js.Json.classify {
      | JSONObject(children) =>
        Ok()
        ->Result.flatMap(_ => decodeField(children, field1))
        ->Result.flatMap(val1 => decodeField(children, field2)->Result.map(val2 => (val1, val2)))
        ->Result.map(construct)
      | _ => Error(#UnexpectedJsonType([], "object", json))
      }
    },
  )

let record3 = (construct, destruct, field1, field2, field3) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2, val3) = destruct(value)
      jsonObject([encodeField(field1, val1), encodeField(field2, val2), encodeField(field3, val3)])
    },
    // decode
    json => {
      switch json->Js.Json.classify {
      | JSONObject(children) =>
        Ok()
        ->Result.flatMap(_ => decodeField(children, field1))
        ->Result.flatMap(val1 => decodeField(children, field2)->Result.map(val2 => (val1, val2)))
        ->Result.flatMap(((val1, val2)) =>
          decodeField(children, field3)->Result.map(val3 => (val1, val2, val3))
        )
        ->Result.map(construct)
      | _ => Error(#UnexpectedJsonType([], "object", json))
      }
    },
  )

let decodeString = (str, codec) => {
  let maybeJson = switch Js.Json.parseExn(str) {
  | json => Ok(json)
  | exception Js.Exn.Error(obj) =>
    let message = Js.Exn.message(obj)
    Error(#SyntaxError(message->Option.getWithDefault("Syntax error")))
  }

  maybeJson->Result.flatMap(json => codec->Codec.decode(json))
}
