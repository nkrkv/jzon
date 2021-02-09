module Error = {
  type t = {
    location: array<string>,
    message: string,
  }

  let make = message => {location: [], message: message}

  let location = ({location}) => location
  let message = ({message}) => message
  let toString = err =>
    (err->location->Js.Array2.joinWith(" -> ") ++ " " ++ err->message)->Js.String2.trim
}

module Codec = {
  type encode<'v> = 'v => Js.Json.t
  type decode<'v> = Js.Json.t => result<'v, string>
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
  | None => Error("Expected string")
  }
)

let float = Codec.make(Js.Json.number, json =>
  switch json->Js.Json.decodeNumber {
  | Some(x) => Ok(x)
  | None => Error("Expected float number")
  }
)

let field = Field.make

let encodeField = (field, val) => (field->Field.key, field->Field.codec->Codec.encode(val))

let decodeField = (jsonObject, field) =>
  switch jsonObject->Js.Dict.get(field->Field.key) {
  | Some(childJson) => field->Field.codec->Codec.decode(childJson)
  | None => Error(j`Expected field "${field->Field.key}"`)
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
      | _ => Error("Expected JSON object")
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
      | _ => Error("Expected JSON object")
      }
    },
  )

let decodeString = (str, codec) => {
  let maybeJson = switch Js.Json.parseExn(str) {
  | json => Ok(json)
  | exception Js.Exn.Error(obj) =>
    let message = Js.Exn.message(obj)
    Error(Error.make(message->Option.getWithDefault("Syntax error")))
  }

  maybeJson->Result.flatMap(json =>
    switch codec->Codec.decode(json) {
    | Ok(_) as ok => ok
    | Error(message) => Error(Error.make(message))
    }
  )
}
