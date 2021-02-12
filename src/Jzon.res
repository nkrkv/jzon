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
    | #UnexpectedJsonValue(location, string)
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
  let decodeString = (codec, str) => {
    let maybeJson = switch Js.Json.parseExn(str) {
    | json => Ok(json)
    | exception Js.Exn.Error(obj) =>
      let message = Js.Exn.message(obj)
      Error(#SyntaxError(message->Option.getWithDefault("Syntax error")))
    }

    maybeJson->Result.flatMap(json => codec->decode(json))
  }

  let identity = make(x => x, x => Ok(x))
}

type codec<'v> = Codec.t<'v>

let encode = Codec.encode
let decode = Codec.decode
let decodeString = Codec.decodeString

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
      | JSONArray(_) =>
        failwith("Field `self` must be encoded as object")
      }
    }

  let decode = (field, fieldset) =>
    switch field->path {
    | Self => field->codec->Codec.decode(Js.Json.object_(fieldset))
    | Key(key) =>
      switch fieldset->Js.Dict.get(key) {
      | Some(childJson) =>
        field
        ->codec
        ->Codec.decode(childJson)
        ->ResultX.mapError(DecodingError.prependLocation(_, Field(key)))
      | None => Error(#MissingField([], key))
      }
    }

  // decode + flatMap the result
  let dfmap = (field, fieldset, fmapFn) => field->decode(fieldset)->Result.flatMap(fmapFn)
}

type field<'v> = Field.t<'v>

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

let int = Codec.make(
  x => float->encode(x->Int.toFloat),
  json => float->decode(json)->Result.map(Js.Math.floor_int),
)

let bool = Codec.make(Js.Json.boolean, json =>
  switch json->Js.Json.decodeBoolean {
  | Some(x) => Ok(x)
  | None => Error(#UnexpectedJsonType([], "bool", json))
  }
)

let json = Codec.identity

let field = (key, codec) => Field.make(Key(key), codec)
let self = Field.make(Self, Codec.identity)

let jsonObject = keyVals => Js.Json.object_(Js.Dict.fromArray(keyVals->Array.concatMany))

let asObject = json =>
  switch json->Js.Json.classify {
  | JSONObject(fieldset) => Ok(fieldset)
  | _ => Error(#UnexpectedJsonType([], "object", json))
  }

let object1 = (destruct, construct, field1) =>
  Codec.make(
    // encode
    value => {
      let val1 = destruct(value)
      jsonObject([Field.encode(field1, val1)])
    },
    // decode
    json =>
      json
      ->asObject
      ->Result.flatMap(fieldset => field1->Field.dfmap(fieldset, val1 => construct(val1))),
  )

let object2 = (destruct, construct, field1, field2) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2) = destruct(value)
      jsonObject([Field.encode(field1, val1), Field.encode(field2, val2)])
    },
    // decode
    json =>
      json
      ->asObject
      ->Result.flatMap(fieldset =>
        field1->Field.dfmap(fieldset, val1 =>
          field2->Field.dfmap(fieldset, val2 => construct((val1, val2)))
        )
      ),
  )

let object3 = (destruct, construct, field1, field2, field3) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2, val3) = destruct(value)
      jsonObject([
        Field.encode(field1, val1),
        Field.encode(field2, val2),
        Field.encode(field3, val3),
      ])
    },
    // decode
    json =>
      json
      ->asObject
      ->Result.flatMap(fieldset =>
        field1->Field.dfmap(fieldset, val1 =>
          field2->Field.dfmap(fieldset, val2 =>
            field3->Field.dfmap(fieldset, val3 => construct((val1, val2, val3)))
          )
        )
      ),
  )

let object15 = (
  destruct,
  construct,
  field1,
  field2,
  field3,
  field4,
  field5,
  field6,
  field7,
  field8,
  field9,
  field10,
  field11,
  field12,
  field13,
  field14,
  field15,
) =>
  Codec.make(
    // encode
    value => {
      let (
        val1,
        val2,
        val3,
        val4,
        val5,
        val6,
        val7,
        val8,
        val9,
        val10,
        val11,
        val12,
        val13,
        val14,
        val15,
      ) = destruct(value)
      jsonObject([
        Field.encode(field1, val1),
        Field.encode(field2, val2),
        Field.encode(field3, val3),
        Field.encode(field4, val4),
        Field.encode(field5, val5),
        Field.encode(field6, val6),
        Field.encode(field7, val7),
        Field.encode(field8, val8),
        Field.encode(field9, val9),
        Field.encode(field10, val10),
        Field.encode(field11, val11),
        Field.encode(field12, val12),
        Field.encode(field13, val13),
        Field.encode(field14, val14),
        Field.encode(field15, val15),
      ])
    },
    // decode
    json =>
      json
      ->asObject
      ->Result.flatMap(fieldset =>
        switch field1->Field.decode(fieldset) {
        | Ok(val1) =>
          switch field2->Field.decode(fieldset) {
          | Ok(val2) =>
            switch field3->Field.decode(fieldset) {
            | Ok(val3) =>
              switch field4->Field.decode(fieldset) {
              | Ok(val4) =>
                switch field5->Field.decode(fieldset) {
                | Ok(val5) =>
                  switch field6->Field.decode(fieldset) {
                  | Ok(val6) =>
                    switch field7->Field.decode(fieldset) {
                    | Ok(val7) =>
                      switch field8->Field.decode(fieldset) {
                      | Ok(val8) =>
                        switch field9->Field.decode(fieldset) {
                        | Ok(val9) =>
                          switch field10->Field.decode(fieldset) {
                          | Ok(val10) =>
                            switch field11->Field.decode(fieldset) {
                            | Ok(val11) =>
                              switch field12->Field.decode(fieldset) {
                              | Ok(val12) =>
                                switch field13->Field.decode(fieldset) {
                                | Ok(val13) =>
                                  switch field14->Field.decode(fieldset) {
                                  | Ok(val14) =>
                                    switch field15->Field.decode(fieldset) {
                                    | Ok(val15) =>
                                      construct((
                                        val1,
                                        val2,
                                        val3,
                                        val4,
                                        val5,
                                        val6,
                                        val7,
                                        val8,
                                        val9,
                                        val10,
                                        val11,
                                        val12,
                                        val13,
                                        val14,
                                        val15,
                                      ))
                                    | Error(_) as err => err
                                    }
                                  | Error(_) as err => err
                                  }
                                | Error(_) as err => err
                                }
                              | Error(_) as err => err
                              }
                            | Error(_) as err => err
                            }
                          | Error(_) as err => err
                          }
                        | Error(_) as err => err
                        }
                      | Error(_) as err => err
                      }
                    | Error(_) as err => err
                    }
                  | Error(_) as err => err
                  }
                | Error(_) as err => err
                }
              | Error(_) as err => err
              }
            | Error(_) as err => err
            }
          | Error(_) as err => err
          }
        | Error(_) as err => err
        }
      ),
  )
