let equals = (~message=?, left, right) =>
  Test.assertion((left, right) => left == right, left, right, ~operator="left == right", ~message?)

let okOf = (~message=?, left, right) =>
  Test.assertion(
    (left, right) => left == Ok(right),
    left,
    right,
    ~operator="left == Ok(right)",
    ~message?,
  )

let roundtrips = (~message=?, data, codec) => {
  let json = codec->Jzon.encode(data)
  let result = codec->Jzon.decode(json)
  result->okOf(data, ~message?)
}

let errorString = (~message=?, left, right) => Test.assertion((left, right) => {
    switch left {
    | Ok(_) => false
    | Error(err) => err->Jzon.DecodingError.toString == right
    }
  }, left, right, ~operator="error string of left == right", ~message?)
