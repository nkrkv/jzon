module ResultX = {
  let mapError = (result, fn) =>
    switch result {
    | Ok(_) as ok => ok
    | Error(err) => Error(fn(err))
    }

  let sequence = (results: array<result<'ok, 'err>>): result<array<'ok>, 'err> => {
    results->Array.reduce(Ok([]), (maybeAcc, res) => {
      maybeAcc->Result.flatMap(acc => res->Result.map(x => acc->Array.concat([x])))
    })
  }
}

module DecodingError = {
  type locationComponent = Field(string) | Index(int)

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
      | Index(index) => `[` ++ index->Int.toString ++ `]`
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
  let encodeString = (codec, value) => codec->encode(value)->Js.Json.stringify

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
let encodeString = Codec.encodeString

let decode = Codec.decode
let decodeString = Codec.decodeString

let custom = Codec.make

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
  json =>
    float
    ->decode(json)
    ->Result.flatMap(x =>
      x == x->Js.Math.trunc && x >= -2147483648. && x <= 2147483647.
        ? Ok(x->Js.Math.unsafe_trunc)
        : Error(#UnexpectedJsonValue([], x->Float.toString))
    ),
)

let bool = Codec.make(Js.Json.boolean, json =>
  switch json->Js.Json.decodeBoolean {
  | Some(x) => Ok(x)
  | None => Error(#UnexpectedJsonType([], "bool", json))
  }
)

let json = Codec.identity

let nullable = payloadCodec =>
  Codec.make(
    maybeValue =>
      switch maybeValue {
      | Some(value) => payloadCodec->encode(value)
      | None => Js.Json.null
      },
    json => json == Js.Json.null ? Ok(None) : payloadCodec->decode(json)->Result.map(v => Some(v)),
  )

let nullAs = (payloadCodec, fallbackValue) =>
  Codec.make(
    value => payloadCodec->encode(value),
    json => json == Js.Json.null ? Ok(fallbackValue) : payloadCodec->decode(json),
  )

let array = elementCodec =>
  Codec.make(
    xs => xs->Array.map(elementCodec->encode(_))->Js.Json.array,
    json =>
      switch json->Js.Json.classify {
      | JSONArray(elementJsons) =>
        elementJsons
        ->Array.mapWithIndex((i, elemJson) =>
          elementCodec
          ->decode(elemJson)
          ->ResultX.mapError(DecodingError.prependLocation(_, Index(i)))
        )
        ->ResultX.sequence
      | _ => Error(#UnexpectedJsonType([], "array", json))
      },
  )

module Field = {
  type path =
    | Self
    | Key(string)

  type claim<'v> =
    | Required
    | Optional
    | OptionalWithDefault('v)

  type t<'v> = {
    path: path,
    codec: Codec.t<'v>,
    claim: claim<'v>,
  }

  let make = (path, codec) => {path: path, codec: codec, claim: Required}
  let makeOptional = ({path, codec}) => {path: path, codec: codec->nullable, claim: Optional}
  let assignDefault = ({path, codec}, value) => {
    path: path,
    codec: codec->nullAs(value),
    claim: OptionalWithDefault(value),
  }

  let path = ({path}) => path
  let codec = ({codec}) => codec
  let claim = ({claim}) => claim

  let encode = (field, val) =>
    switch field->path {
    | Key(key) =>
      let json = field->codec->Codec.encode(val)
      json == Js.Json.null && field->claim == Optional ? [] : [(key, json)]
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
      let decodeChild = childJson =>
        field
        ->codec
        ->Codec.decode(childJson)
        ->ResultX.mapError(DecodingError.prependLocation(_, Field(key)))

      switch (fieldset->Js.Dict.get(key), field->claim) {
      | (Some(childJson), _) => decodeChild(childJson)
      | (None, Optional) => decodeChild(Js.Json.null)
      | (None, OptionalWithDefault(x)) => Ok(x)
      | (None, Required) => Error(#MissingField([], key))
      }
    }
}

type field<'v> = Field.t<'v>

let field = (key, codec) => Field.make(Key(key), codec)
let self = Field.make(Self, Codec.identity)
let optional = Field.makeOptional
let default = Field.assignDefault

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
      ->Result.flatMap(fieldset =>
        switch field1->Field.decode(fieldset) {
        | Ok(val1) => construct(val1)
        | Error(_) as err => err
        }
      ),
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
        switch field1->Field.decode(fieldset) {
        | Ok(val1) =>
          switch field2->Field.decode(fieldset) {
          | Ok(val2) => construct((val1, val2))
          | Error(_) as err => err
          }
        | Error(_) as err => err
        }
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
        switch field1->Field.decode(fieldset) {
        | Ok(val1) =>
          switch field2->Field.decode(fieldset) {
          | Ok(val2) =>
            switch field3->Field.decode(fieldset) {
            | Ok(val3) => construct((val1, val2, val3))
            | Error(_) as err => err
            }
          | Error(_) as err => err
          }
        | Error(_) as err => err
        }
      ),
  )

let object4 = (destruct, construct, field1, field2, field3, field4) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2, val3, val4) = destruct(value)
      jsonObject([
        Field.encode(field1, val1),
        Field.encode(field2, val2),
        Field.encode(field3, val3),
        Field.encode(field4, val4),
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
              | Ok(val4) => construct((val1, val2, val3, val4))
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

let object5 = (destruct, construct, field1, field2, field3, field4, field5) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2, val3, val4, val5) = destruct(value)
      jsonObject([
        Field.encode(field1, val1),
        Field.encode(field2, val2),
        Field.encode(field3, val3),
        Field.encode(field4, val4),
        Field.encode(field5, val5),
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
                | Ok(val5) => construct((val1, val2, val3, val4, val5))
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

let object6 = (destruct, construct, field1, field2, field3, field4, field5, field6) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2, val3, val4, val5, val6) = destruct(value)
      jsonObject([
        Field.encode(field1, val1),
        Field.encode(field2, val2),
        Field.encode(field3, val3),
        Field.encode(field4, val4),
        Field.encode(field5, val5),
        Field.encode(field6, val6),
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
                  | Ok(val6) => construct((val1, val2, val3, val4, val5, val6))
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

let object7 = (destruct, construct, field1, field2, field3, field4, field5, field6, field7) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2, val3, val4, val5, val6, val7) = destruct(value)
      jsonObject([
        Field.encode(field1, val1),
        Field.encode(field2, val2),
        Field.encode(field3, val3),
        Field.encode(field4, val4),
        Field.encode(field5, val5),
        Field.encode(field6, val6),
        Field.encode(field7, val7),
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
                    | Ok(val7) => construct((val1, val2, val3, val4, val5, val6, val7))
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

let object8 = (
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
) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2, val3, val4, val5, val6, val7, val8) = destruct(value)
      jsonObject([
        Field.encode(field1, val1),
        Field.encode(field2, val2),
        Field.encode(field3, val3),
        Field.encode(field4, val4),
        Field.encode(field5, val5),
        Field.encode(field6, val6),
        Field.encode(field7, val7),
        Field.encode(field8, val8),
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
                      | Ok(val8) => construct((val1, val2, val3, val4, val5, val6, val7, val8))
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

let object9 = (
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
) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2, val3, val4, val5, val6, val7, val8, val9) = destruct(value)
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
                          construct((val1, val2, val3, val4, val5, val6, val7, val8, val9))
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

let object10 = (
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
) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2, val3, val4, val5, val6, val7, val8, val9, val10) = destruct(value)
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
                            construct((val1, val2, val3, val4, val5, val6, val7, val8, val9, val10))
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

let object11 = (
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
) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2, val3, val4, val5, val6, val7, val8, val9, val10, val11) = destruct(value)
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
      ),
  )

let object12 = (
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
) =>
  Codec.make(
    // encode
    value => {
      let (val1, val2, val3, val4, val5, val6, val7, val8, val9, val10, val11, val12) = destruct(
        value,
      )
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
      ),
  )

let object13 = (
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
      ),
  )

let object14 = (
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
