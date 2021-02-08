module Error = {
  type t = {
    location: array<string>,
    message: string,
  }

  let make = message => {location: [], message: message}

  let location = ({location}) => location
  let message = ({message}) => message
  let toString = err => err->location->Js.Array2.joinWith(" -> ") ++ " " ++ err->message
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

let record2 = (construct, destruct, field1, field2) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2) = destruct(value)
      Js.Json.object_(
        Js.Dict.fromArray([
          (field1->Field.key, field1->Field.codec->Codec.encode(val1)),
          (field2->Field.key, field2->Field.codec->Codec.encode(val2)),
        ]),
      )
    },
    // decode
    json => {
      switch json->Js.Json.classify {
      | JSONObject(children) =>
        Ok()
        ->Result.flatMap(_ => {
          let val = switch children->Js.Dict.get(field1->Field.key) {
          | Some(childJson) => field1->Field.codec->Codec.decode(childJson)
          | None => Error(j`Expected field "${field1->Field.key}"`)
          }

          val
        })
        ->Result.flatMap(val1 => {
          let val = switch children->Js.Dict.get(field2->Field.key) {
          | Some(childJson) => field2->Field.codec->Codec.decode(childJson)
          | None => Error(j`Expected field "${field2->Field.key}"`)
          }

          val->Result.map(val2 => (val1, val2))
        })
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
      Js.Json.object_(
        Js.Dict.fromArray([
          (field1->Field.key, field1->Field.codec->Codec.encode(val1)),
          (field2->Field.key, field2->Field.codec->Codec.encode(val2)),
          (field3->Field.key, field3->Field.codec->Codec.encode(val3)),
        ]),
      )
    },
    // decode
    json => {
      switch json->Js.Json.classify {
      | JSONObject(children) =>
        Ok()
        ->Result.flatMap(_ => {
          let val = switch children->Js.Dict.get(field1->Field.key) {
          | Some(childJson) => field1->Field.codec->Codec.decode(childJson)
          | None => Error(j`Expected field "${field1->Field.key}"`)
          }

          val
        })
        ->Result.flatMap(val1 => {
          let val = switch children->Js.Dict.get(field2->Field.key) {
          | Some(childJson) => field2->Field.codec->Codec.decode(childJson)
          | None => Error(j`Expected field "${field2->Field.key}"`)
          }

          val->Result.map(val2 => (val1, val2))
        })
        ->Result.flatMap(((val1, val2)) => {
          let val = switch children->Js.Dict.get(field3->Field.key) {
          | Some(childJson) => field3->Field.codec->Codec.decode(childJson)
          | None => Error(j`Expected field "${field3->Field.key}"`)
          }

          val->Result.map(val3 => (val1, val2, val3))
        })
        ->Result.map(construct)
      | _ => Error("Expected JSON object")
      }
    },
  )

let decodeString = (str, codec) =>
  switch codec->Codec.decode(Js.Json.parseExn(str)) {
  | Ok(_) as ok => ok
  | Error(message) => Error(Error.make(message))
  }
