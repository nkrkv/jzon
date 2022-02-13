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
let encodeWith = (data, codec) => codec->encode(data)
let encodeString = Codec.encodeString
let encodeStringWith = (data, codec) => codec->encodeString(data)

let decode = Codec.decode
let decodeWith = (json, codec) => codec->decode(json)
let decodeString = Codec.decodeString
let decodeStringWith = (string, codec) => codec->decodeString(string)

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

let asObject = json =>
  switch json->Js.Json.classify {
  | JSONObject(fieldset) => Ok(fieldset)
  | _ => Error(#UnexpectedJsonType([], "object", json))
  }

let dict = valuesCodec =>
  Codec.make(
    dict => dict->Js.Dict.map((. val) => valuesCodec->encode(val), _)->Js.Json.object_,
    json =>
      json
      ->asObject
      ->Result.flatMap(obj => {
        let (keys, valResults) =
          obj
          ->Js.Dict.entries
          ->Array.map(((key, val)) => (
            key,
            valuesCodec
            ->decode(val)
            ->ResultX.mapError(DecodingError.prependLocation(_, Field(key))),
          ))
          ->Array.unzip

        valResults->ResultX.sequence->Result.map(vals => Array.zip(keys, vals)->Js.Dict.fromArray)
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

@inline
let ap = (fn, r) => fn->Result.flatMap(fn => r->Result.map(fn))

@inline
let unpack = r =>
  switch r {
  | Ok(data) => data
  | Error(_) as err => err
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((val1, val2) => construct((val1, val2)))
        ->ap(field2->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((val1, val2, val3) => construct((val1, val2, val3)))
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((val1, val2, val3, val4) => construct((val1, val2, val3, val4)))
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((val1, val2, val3, val4, val5) => construct((val1, val2, val3, val4, val5)))
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((val1, val2, val3, val4, val5, val6) =>
          construct((val1, val2, val3, val4, val5, val6))
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((val1, val2, val3, val4, val5, val6, val7) =>
          construct((val1, val2, val3, val4, val5, val6, val7))
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((val1, val2, val3, val4, val5, val6, val7, val8) =>
          construct((val1, val2, val3, val4, val5, val6, val7, val8))
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((val1, val2, val3, val4, val5, val6, val7, val8, val9) =>
          construct((val1, val2, val3, val4, val5, val6, val7, val8, val9))
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((val1, val2, val3, val4, val5, val6, val7, val8, val9, val10) =>
          construct((val1, val2, val3, val4, val5, val6, val7, val8, val9, val10))
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((val1, val2, val3, val4, val5, val6, val7, val8, val9, val10, val11) =>
          construct((val1, val2, val3, val4, val5, val6, val7, val8, val9, val10, val11))
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((val1, val2, val3, val4, val5, val6, val7, val8, val9, val10, val11, val12) =>
          construct((val1, val2, val3, val4, val5, val6, val7, val8, val9, val10, val11, val12))
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->ap(field12->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((
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
        ) =>
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
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->ap(field12->Field.decode(fieldset))
        ->ap(field13->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((
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
        ) =>
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
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->ap(field12->Field.decode(fieldset))
        ->ap(field13->Field.decode(fieldset))
        ->ap(field14->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((
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
        ) =>
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
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->ap(field12->Field.decode(fieldset))
        ->ap(field13->Field.decode(fieldset))
        ->ap(field14->Field.decode(fieldset))
        ->ap(field15->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((
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
        ) =>
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
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->ap(field12->Field.decode(fieldset))
        ->ap(field13->Field.decode(fieldset))
        ->ap(field14->Field.decode(fieldset))
        ->ap(field15->Field.decode(fieldset))
        ->ap(field16->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((
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
        ) =>
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
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->ap(field12->Field.decode(fieldset))
        ->ap(field13->Field.decode(fieldset))
        ->ap(field14->Field.decode(fieldset))
        ->ap(field15->Field.decode(fieldset))
        ->ap(field16->Field.decode(fieldset))
        ->ap(field17->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((
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
        ) =>
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
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->ap(field12->Field.decode(fieldset))
        ->ap(field13->Field.decode(fieldset))
        ->ap(field14->Field.decode(fieldset))
        ->ap(field15->Field.decode(fieldset))
        ->ap(field16->Field.decode(fieldset))
        ->ap(field17->Field.decode(fieldset))
        ->ap(field18->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((
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
        ) =>
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
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->ap(field12->Field.decode(fieldset))
        ->ap(field13->Field.decode(fieldset))
        ->ap(field14->Field.decode(fieldset))
        ->ap(field15->Field.decode(fieldset))
        ->ap(field16->Field.decode(fieldset))
        ->ap(field17->Field.decode(fieldset))
        ->ap(field18->Field.decode(fieldset))
        ->ap(field19->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((
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
        ) =>
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
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->ap(field12->Field.decode(fieldset))
        ->ap(field13->Field.decode(fieldset))
        ->ap(field14->Field.decode(fieldset))
        ->ap(field15->Field.decode(fieldset))
        ->ap(field16->Field.decode(fieldset))
        ->ap(field17->Field.decode(fieldset))
        ->ap(field18->Field.decode(fieldset))
        ->ap(field19->Field.decode(fieldset))
        ->ap(field20->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((
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
        ) =>
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
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->ap(field12->Field.decode(fieldset))
        ->ap(field13->Field.decode(fieldset))
        ->ap(field14->Field.decode(fieldset))
        ->ap(field15->Field.decode(fieldset))
        ->ap(field16->Field.decode(fieldset))
        ->ap(field17->Field.decode(fieldset))
        ->ap(field18->Field.decode(fieldset))
        ->ap(field19->Field.decode(fieldset))
        ->ap(field20->Field.decode(fieldset))
        ->ap(field21->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((
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
        ) =>
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
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->ap(field12->Field.decode(fieldset))
        ->ap(field13->Field.decode(fieldset))
        ->ap(field14->Field.decode(fieldset))
        ->ap(field15->Field.decode(fieldset))
        ->ap(field16->Field.decode(fieldset))
        ->ap(field17->Field.decode(fieldset))
        ->ap(field18->Field.decode(fieldset))
        ->ap(field19->Field.decode(fieldset))
        ->ap(field20->Field.decode(fieldset))
        ->ap(field21->Field.decode(fieldset))
        ->ap(field22->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((
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
        ) =>
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
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->ap(field12->Field.decode(fieldset))
        ->ap(field13->Field.decode(fieldset))
        ->ap(field14->Field.decode(fieldset))
        ->ap(field15->Field.decode(fieldset))
        ->ap(field16->Field.decode(fieldset))
        ->ap(field17->Field.decode(fieldset))
        ->ap(field18->Field.decode(fieldset))
        ->ap(field19->Field.decode(fieldset))
        ->ap(field20->Field.decode(fieldset))
        ->ap(field21->Field.decode(fieldset))
        ->ap(field22->Field.decode(fieldset))
        ->ap(field23->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((
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
        ) =>
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
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->ap(field12->Field.decode(fieldset))
        ->ap(field13->Field.decode(fieldset))
        ->ap(field14->Field.decode(fieldset))
        ->ap(field15->Field.decode(fieldset))
        ->ap(field16->Field.decode(fieldset))
        ->ap(field17->Field.decode(fieldset))
        ->ap(field18->Field.decode(fieldset))
        ->ap(field19->Field.decode(fieldset))
        ->ap(field20->Field.decode(fieldset))
        ->ap(field21->Field.decode(fieldset))
        ->ap(field22->Field.decode(fieldset))
        ->ap(field23->Field.decode(fieldset))
        ->ap(field24->Field.decode(fieldset))
        ->unpack
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
        field1
        ->Field.decode(fieldset)
        ->Result.map((
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
        ) =>
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
        )
        ->ap(field2->Field.decode(fieldset))
        ->ap(field3->Field.decode(fieldset))
        ->ap(field4->Field.decode(fieldset))
        ->ap(field5->Field.decode(fieldset))
        ->ap(field6->Field.decode(fieldset))
        ->ap(field7->Field.decode(fieldset))
        ->ap(field8->Field.decode(fieldset))
        ->ap(field9->Field.decode(fieldset))
        ->ap(field10->Field.decode(fieldset))
        ->ap(field11->Field.decode(fieldset))
        ->ap(field12->Field.decode(fieldset))
        ->ap(field13->Field.decode(fieldset))
        ->ap(field14->Field.decode(fieldset))
        ->ap(field15->Field.decode(fieldset))
        ->ap(field16->Field.decode(fieldset))
        ->ap(field17->Field.decode(fieldset))
        ->ap(field18->Field.decode(fieldset))
        ->ap(field19->Field.decode(fieldset))
        ->ap(field20->Field.decode(fieldset))
        ->ap(field21->Field.decode(fieldset))
        ->ap(field22->Field.decode(fieldset))
        ->ap(field23->Field.decode(fieldset))
        ->ap(field24->Field.decode(fieldset))
        ->ap(field25->Field.decode(fieldset))
        ->unpack
      ),
  )
