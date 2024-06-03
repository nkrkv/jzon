module ResultX = {
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
    | #UnexpectedJsonType(location, string, JSON.t)
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
    ->Array.joinWith(".")

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
      let actualType = switch actualJson->JSON.Classify.classify {
      | Bool(_) => "boolean"
      | Null => "null"
      | String(_) => "string"
      | Number(_) => "number"
      | Object(_) => "object"
      | Array(_) => "array"
      }

      `Expected ${expectation}, got ${actualType} at ${location->formatLocation}`
    | #UnexpectedJsonValue(location, found) =>
      `Unexpected value ${found} at ${location->formatLocation}`
    }
}

module Codec = {
  type encode<'v> = 'v => JSON.t
  type decode<'v> = JSON.t => result<'v, DecodingError.t>
  type t<'v> = {
    encode: encode<'v>,
    decode: decode<'v>,
  }

  let make = (encode, decode) => {encode, decode}

  let encode = (codec, value) => codec.encode(value)
  let encodeString = (codec, value) => codec->encode(value)->JSON.stringify

  let decode = (codec, value) => codec.decode(value)
  let decodeString = (codec, str) => {
    let maybeJson = switch JSON.parseExn(str) {
    | json => Ok(json)
    | exception Exn.Error(obj) =>
      let message = Error.message(obj)
      Error(#SyntaxError(message->Option.getOr("Syntax error")))
    }

    maybeJson->Result.flatMap(json => codec->decode(json))
  }

  let identity = make(x => x, x => Ok(x))
}

type codec<'v> = Codec.t<'v>

let encode = Codec.encode
let encodeWith = (data, codec) => codec->encode(data)
let encodeString = Codec.encodeString
let encodeStringWith = (data, codec) => codec->encodeString(data)

let decode = Codec.decode
let decodeWith = (json, codec) => codec->decode(json)
let decodeString = Codec.decodeString
let decodeStringWith = (string, codec) => codec->decodeString(string)

let custom = Codec.make

let string = Codec.make(JSON.Encode.string, json =>
  switch json->JSON.Decode.string {
  | Some(x) => Ok(x)
  | None => Error(#UnexpectedJsonType([], "string", json))
  }
)

let float = Codec.make(JSON.Encode.float, json =>
  switch json->JSON.Decode.float {
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
      x == x->Math.trunc &&
      x >= Int.Constants.minValue->Int.toFloat &&
      x <= Int.Constants.maxValue->Int.toFloat
        ? Ok(x->Int.fromFloat)
        : Error(#UnexpectedJsonValue([], x->Float.toString))
    ),
)

let bool = Codec.make(JSON.Encode.bool, json =>
  switch json->JSON.Decode.bool {
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
      | None => JSON.Encode.null
      },
    json =>
      json == JSON.Encode.null ? Ok(None) : payloadCodec->decode(json)->Result.map(v => Some(v)),
  )

let nullAs = (payloadCodec, fallbackValue) =>
  Codec.make(
    value => payloadCodec->encode(value),
    json => json == JSON.Encode.null ? Ok(fallbackValue) : payloadCodec->decode(json),
  )

let array = elementCodec =>
  Codec.make(
    xs => xs->Array.map(x => elementCodec->encode(x))->JSON.Encode.array,
    json =>
      switch json->JSON.Classify.classify {
      | Array(elementJsons) =>
        elementJsons
        ->Array.mapWithIndex((elemJson, i) =>
          elementCodec
          ->decode(elemJson)
          ->Result.mapError(DecodingError.prependLocation(_, Index(i)))
        )
        ->ResultX.sequence
      | _ => Error(#UnexpectedJsonType([], "array", json))
      },
  )

let asObject = json =>
  switch json->JSON.Classify.classify {
  | Object(fieldset) => Ok(fieldset)
  | _ => Error(#UnexpectedJsonType([], "object", json))
  }

let dict = valuesCodec =>
  Codec.make(
    dict => dict->Dict.mapValues(val => valuesCodec->encode(val))->JSON.Encode.object,
    json =>
      json
      ->asObject
      ->Result.flatMap(obj => {
        let (keys, valResults) =
          obj
          ->Dict.toArray
          ->Array.map(((key, val)) => (
            key,
            valuesCodec
            ->decode(val)
            ->Result.mapError(DecodingError.prependLocation(_, Field(key))),
          ))
          ->Belt.Array.unzip

        valResults->ResultX.sequence->Result.map(vals => Belt.Array.zip(keys, vals)->Dict.fromArray)
      }),
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

  let make = (path, codec) => {path, codec, claim: Required}
  let makeOptional = ({path, codec}) => {path, codec: codec->nullable, claim: Optional}
  let assignDefault = ({path, codec}, value) => {
    path,
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
      json == JSON.Encode.null && field->claim == Optional ? [] : [(key, json)]
    | Self =>
      switch field->codec->Codec.encode(val)->JSON.Classify.classify {
      | Object(objDict) => objDict->Dict.toArray
      | Bool(_)
      | Null
      | String(_)
      | Number(_)
      | Array(_) =>
        failwith("Field `self` must be encoded as object")
      }
    }

  let decode = (field, fieldset) =>
    switch field->path {
    | Self => field->codec->Codec.decode(JSON.Encode.object(fieldset))
    | Key(key) =>
      let decodeChild = childJson =>
        field
        ->codec
        ->Codec.decode(childJson)
        ->Result.mapError(DecodingError.prependLocation(_, Field(key)))

      switch (fieldset->Dict.get(key), field->claim) {
      | (Some(childJson), _) => decodeChild(childJson)
      | (None, Optional) => decodeChild(JSON.Encode.null)
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

let jsonObject = keyVals => JSON.Encode.object(Dict.fromArray([]->Array.concatMany(keyVals)))

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

let object16 = (
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
  field16,
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
        val16,
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
        Field.encode(field16, val16),
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
                                      switch field16->Field.decode(fieldset) {
                                      | Ok(val16) =>
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
                                          val16,
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
        | Error(_) as err => err
        }
      ),
  )

let object17 = (
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
  field16,
  field17,
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
        val16,
        val17,
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
        Field.encode(field16, val16),
        Field.encode(field17, val17),
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
                                      switch field16->Field.decode(fieldset) {
                                      | Ok(val16) =>
                                        switch field17->Field.decode(fieldset) {
                                        | Ok(val17) =>
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
                                            val16,
                                            val17,
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
          | Error(_) as err => err
          }
        | Error(_) as err => err
        }
      ),
  )

let object18 = (
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
  field16,
  field17,
  field18,
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
        val16,
        val17,
        val18,
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
        Field.encode(field16, val16),
        Field.encode(field17, val17),
        Field.encode(field18, val18),
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
                                      switch field16->Field.decode(fieldset) {
                                      | Ok(val16) =>
                                        switch field17->Field.decode(fieldset) {
                                        | Ok(val17) =>
                                          switch field18->Field.decode(fieldset) {
                                          | Ok(val18) =>
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
                                              val16,
                                              val17,
                                              val18,
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
            | Error(_) as err => err
            }
          | Error(_) as err => err
          }
        | Error(_) as err => err
        }
      ),
  )

let object19 = (
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
  field16,
  field17,
  field18,
  field19,
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
        val16,
        val17,
        val18,
        val19,
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
        Field.encode(field16, val16),
        Field.encode(field17, val17),
        Field.encode(field18, val18),
        Field.encode(field19, val19),
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
                                      switch field16->Field.decode(fieldset) {
                                      | Ok(val16) =>
                                        switch field17->Field.decode(fieldset) {
                                        | Ok(val17) =>
                                          switch field18->Field.decode(fieldset) {
                                          | Ok(val18) =>
                                            switch field19->Field.decode(fieldset) {
                                            | Ok(val19) =>
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
                                                val16,
                                                val17,
                                                val18,
                                                val19,
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

let object20 = (
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
  field16,
  field17,
  field18,
  field19,
  field20,
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
        val16,
        val17,
        val18,
        val19,
        val20,
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
        Field.encode(field16, val16),
        Field.encode(field17, val17),
        Field.encode(field18, val18),
        Field.encode(field19, val19),
        Field.encode(field20, val20),
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
                                      switch field16->Field.decode(fieldset) {
                                      | Ok(val16) =>
                                        switch field17->Field.decode(fieldset) {
                                        | Ok(val17) =>
                                          switch field18->Field.decode(fieldset) {
                                          | Ok(val18) =>
                                            switch field19->Field.decode(fieldset) {
                                            | Ok(val19) =>
                                              switch field20->Field.decode(fieldset) {
                                              | Ok(val20) =>
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
                                                  val16,
                                                  val17,
                                                  val18,
                                                  val19,
                                                  val20,
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

let object21 = (
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
  field16,
  field17,
  field18,
  field19,
  field20,
  field21,
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
        val16,
        val17,
        val18,
        val19,
        val20,
        val21,
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
        Field.encode(field16, val16),
        Field.encode(field17, val17),
        Field.encode(field18, val18),
        Field.encode(field19, val19),
        Field.encode(field20, val20),
        Field.encode(field21, val21),
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
                                      switch field16->Field.decode(fieldset) {
                                      | Ok(val16) =>
                                        switch field17->Field.decode(fieldset) {
                                        | Ok(val17) =>
                                          switch field18->Field.decode(fieldset) {
                                          | Ok(val18) =>
                                            switch field19->Field.decode(fieldset) {
                                            | Ok(val19) =>
                                              switch field20->Field.decode(fieldset) {
                                              | Ok(val20) =>
                                                switch field21->Field.decode(fieldset) {
                                                | Ok(val21) =>
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
                                                    val16,
                                                    val17,
                                                    val18,
                                                    val19,
                                                    val20,
                                                    val21,
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

let object22 = (
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
  field16,
  field17,
  field18,
  field19,
  field20,
  field21,
  field22,
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
        val16,
        val17,
        val18,
        val19,
        val20,
        val21,
        val22,
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
        Field.encode(field16, val16),
        Field.encode(field17, val17),
        Field.encode(field18, val18),
        Field.encode(field19, val19),
        Field.encode(field20, val20),
        Field.encode(field21, val21),
        Field.encode(field22, val22),
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
                                      switch field16->Field.decode(fieldset) {
                                      | Ok(val16) =>
                                        switch field17->Field.decode(fieldset) {
                                        | Ok(val17) =>
                                          switch field18->Field.decode(fieldset) {
                                          | Ok(val18) =>
                                            switch field19->Field.decode(fieldset) {
                                            | Ok(val19) =>
                                              switch field20->Field.decode(fieldset) {
                                              | Ok(val20) =>
                                                switch field21->Field.decode(fieldset) {
                                                | Ok(val21) =>
                                                  switch field22->Field.decode(fieldset) {
                                                  | Ok(val22) =>
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
                                                      val16,
                                                      val17,
                                                      val18,
                                                      val19,
                                                      val20,
                                                      val21,
                                                      val22,
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

let object23 = (
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
  field16,
  field17,
  field18,
  field19,
  field20,
  field21,
  field22,
  field23,
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
        val16,
        val17,
        val18,
        val19,
        val20,
        val21,
        val22,
        val23,
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
        Field.encode(field16, val16),
        Field.encode(field17, val17),
        Field.encode(field18, val18),
        Field.encode(field19, val19),
        Field.encode(field20, val20),
        Field.encode(field21, val21),
        Field.encode(field22, val22),
        Field.encode(field23, val23),
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
                                      switch field16->Field.decode(fieldset) {
                                      | Ok(val16) =>
                                        switch field17->Field.decode(fieldset) {
                                        | Ok(val17) =>
                                          switch field18->Field.decode(fieldset) {
                                          | Ok(val18) =>
                                            switch field19->Field.decode(fieldset) {
                                            | Ok(val19) =>
                                              switch field20->Field.decode(fieldset) {
                                              | Ok(val20) =>
                                                switch field21->Field.decode(fieldset) {
                                                | Ok(val21) =>
                                                  switch field22->Field.decode(fieldset) {
                                                  | Ok(val22) =>
                                                    switch field23->Field.decode(fieldset) {
                                                    | Ok(val23) =>
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
                                                        val16,
                                                        val17,
                                                        val18,
                                                        val19,
                                                        val20,
                                                        val21,
                                                        val22,
                                                        val23,
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

let object24 = (
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
  field16,
  field17,
  field18,
  field19,
  field20,
  field21,
  field22,
  field23,
  field24,
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
        val16,
        val17,
        val18,
        val19,
        val20,
        val21,
        val22,
        val23,
        val24,
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
        Field.encode(field16, val16),
        Field.encode(field17, val17),
        Field.encode(field18, val18),
        Field.encode(field19, val19),
        Field.encode(field20, val20),
        Field.encode(field21, val21),
        Field.encode(field22, val22),
        Field.encode(field23, val23),
        Field.encode(field24, val24),
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
                                      switch field16->Field.decode(fieldset) {
                                      | Ok(val16) =>
                                        switch field17->Field.decode(fieldset) {
                                        | Ok(val17) =>
                                          switch field18->Field.decode(fieldset) {
                                          | Ok(val18) =>
                                            switch field19->Field.decode(fieldset) {
                                            | Ok(val19) =>
                                              switch field20->Field.decode(fieldset) {
                                              | Ok(val20) =>
                                                switch field21->Field.decode(fieldset) {
                                                | Ok(val21) =>
                                                  switch field22->Field.decode(fieldset) {
                                                  | Ok(val22) =>
                                                    switch field23->Field.decode(fieldset) {
                                                    | Ok(val23) =>
                                                      switch field24->Field.decode(fieldset) {
                                                      | Ok(val24) =>
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
                                                          val16,
                                                          val17,
                                                          val18,
                                                          val19,
                                                          val20,
                                                          val21,
                                                          val22,
                                                          val23,
                                                          val24,
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

let object25 = (
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
  field16,
  field17,
  field18,
  field19,
  field20,
  field21,
  field22,
  field23,
  field24,
  field25,
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
        val16,
        val17,
        val18,
        val19,
        val20,
        val21,
        val22,
        val23,
        val24,
        val25,
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
        Field.encode(field16, val16),
        Field.encode(field17, val17),
        Field.encode(field18, val18),
        Field.encode(field19, val19),
        Field.encode(field20, val20),
        Field.encode(field21, val21),
        Field.encode(field22, val22),
        Field.encode(field23, val23),
        Field.encode(field24, val24),
        Field.encode(field25, val25),
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
                                      switch field16->Field.decode(fieldset) {
                                      | Ok(val16) =>
                                        switch field17->Field.decode(fieldset) {
                                        | Ok(val17) =>
                                          switch field18->Field.decode(fieldset) {
                                          | Ok(val18) =>
                                            switch field19->Field.decode(fieldset) {
                                            | Ok(val19) =>
                                              switch field20->Field.decode(fieldset) {
                                              | Ok(val20) =>
                                                switch field21->Field.decode(fieldset) {
                                                | Ok(val21) =>
                                                  switch field22->Field.decode(fieldset) {
                                                  | Ok(val22) =>
                                                    switch field23->Field.decode(fieldset) {
                                                    | Ok(val23) =>
                                                      switch field24->Field.decode(fieldset) {
                                                      | Ok(val24) =>
                                                        switch field25->Field.decode(fieldset) {
                                                        | Ok(val25) =>
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
                                                            val16,
                                                            val17,
                                                            val18,
                                                            val19,
                                                            val20,
                                                            val21,
                                                            val22,
                                                            val23,
                                                            val24,
                                                            val25,
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
