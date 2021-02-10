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
    | #MissingField(location, string /* key */)
    | #UnexpectedJsonType(location, string /* expected */, Js.Json.t)
    | #UnexpectedJsonValue(location, string /* found */)
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
    | #UnexpectedJsonValue(location, found) =>
      let location' = [loc]->Array.concat(location)
      #UnexpectedJsonValue(location', found)
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
    | #UnexpectedJsonValue(location, found) =>
      `Unexpected value ${found} at ${location->formatLocation}`
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

  let identity = make(x => x, x => Ok(x));
}

module Field = {
  type path =
    | Self
    | Key(string)

  type t<'v> = {
    path: path,
    codec: Codec.t<'v>,
  }

  let make = (path, codec) => {path: path, codec: codec}
  let path = ({path}) => path
  let codec = ({codec}) => codec

  let encode = (field, val) =>
    switch field->path {
    | Key(key) => [(key, field->codec->Codec.encode(val))]
    | Self =>
      switch field->codec->Codec.encode(val)->Js.Json.classify {
      | JSONObject(objDict) => objDict->Js.Dict.entries
      | JSONFalse
      | JSONTrue
      | JSONNull
      | JSONString(_)
      | JSONNumber(_)
      | JSONArray(_) => failwith("Field `self` must be encoded as object")
      }
    }

  let decode = (objDict, field) =>
    switch field->path {
    | Self => field->codec->Codec.decode(Js.Json.object_(objDict))
    | Key(key) =>
      switch objDict->Js.Dict.get(key) {
      | Some(childJson) =>
        field
        ->codec
        ->Codec.decode(childJson)
        ->ResultX.mapError(DecodingError.prependLocation(_, Field(key)))
      | None => Error(#MissingField([], key))
      }
    }
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

let field = (key, codec) => Field.make(Key(key), codec);
let self = Field.make(Self, Codec.identity);

let jsonObject = keyVals => Js.Json.object_(Js.Dict.fromArray(keyVals->Array.concatMany))

let record1 = (construct, destruct, field1) =>
  Codec.make(
    // encode
    value => {
      let val1 = destruct(value)
      jsonObject([Field.encode(field1, val1)])
    },
    // decode
    json => {
      switch json->Js.Json.classify {
      | JSONObject(children) =>
        Ok()
        ->Result.flatMap(_ => Field.decode(children, field1))
        ->Result.flatMap(construct)
      | _ => Error(#UnexpectedJsonType([], "object", json))
      }
    },
  )

let record2 = (construct, destruct, field1, field2) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2) = destruct(value)
      jsonObject([Field.encode(field1, val1), Field.encode(field2, val2)])
    },
    // decode
    json => {
      switch json->Js.Json.classify {
      | JSONObject(children) =>
        Ok()
        ->Result.flatMap(_ => Field.decode(children, field1))
        ->Result.flatMap(val1 => Field.decode(children, field2)->Result.map(val2 => (val1, val2)))
        ->Result.flatMap(construct)
      | _ => Error(#UnexpectedJsonType([], "object", json))
      }
    },
  )

let record3 = (construct, destruct, field1, field2, field3) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2, val3) = destruct(value)
      jsonObject([Field.encode(field1, val1), Field.encode(field2, val2), Field.encode(field3, val3)])
    },
    // decode
    json => {
      switch json->Js.Json.classify {
      | JSONObject(children) =>
        Ok()
        ->Result.flatMap(_ => Field.decode(children, field1))
        ->Result.flatMap(val1 => Field.decode(children, field2)->Result.map(val2 => (val1, val2)))
        ->Result.flatMap(((val1, val2)) =>
          Field.decode(children, field3)->Result.map(val3 => (val1, val2, val3))
        )
        ->Result.flatMap(construct)
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
