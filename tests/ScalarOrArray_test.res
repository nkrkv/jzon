type dataItem = {
  id: string,
  type_: string,
  payload: string,
}

type message = {data: array<dataItem>}

module ResultX = {
  // This should really belong to the standard lib
  let sequence = (results: array<result<'ok, 'err>>): result<array<'ok>, 'err> => {
    results->Array.reduce(Ok([]), (maybeAcc, res) => {
      maybeAcc->Result.flatMap(acc => res->Result.map(x => acc->Array.concat([x])))
    })
  }
}

module Codecs = {
  // Introduce a new codec for such case
  let scalarOrArray = itemCodec =>
    Jzon.custom(
      items =>
        items->Array.length == 1
          ? Jzon.encode(itemCodec, items->Array.getExn(0))
          : Jzon.encode(Jzon.array(itemCodec), items),
      json =>
        switch json->Js.Json.decodeArray {
        | Some(itemJsons) => itemJsons->Array.map(Jzon.decode(itemCodec))->ResultX.sequence
        | None => Jzon.decode(itemCodec, json)->Result.map(item => [item])
        },
    )

  let dataItem = Jzon.object3(
    ({id, type_, payload}) => (id, type_, payload),
    ((id, type_, payload)) => {id: id, type_: type_, payload: payload}->Ok,
    Jzon.field("id", Jzon.string),
    Jzon.field("type", Jzon.string),
    Jzon.field("payload", Jzon.string),
  )

  let message = Jzon.object1(
    ({data}) => data,
    data => {data: data}->Ok,
    Jzon.field("data", scalarOrArray(dataItem)),
  )
}

Test.test("Scalar or array -> single", () => {
  Codecs.message
  ->Jzon.decodeString(`{
    "data": {"id": "1", "type": "foos", "payload": "zzzz"}
  }`)
  ->Assert.okOf({data: [{id: "1", type_: "foos", payload: "zzzz"}]}, ~message="Decodes correctly")

  Assert.roundtrips(
    {data: [{id: "1", type_: "foos", payload: "zzzz"}]},
    Codecs.message,
    ~message="Does roundtrip",
  )
})

Test.test("Scalar or array -> multiple", () => {
  Codecs.message
  ->Jzon.decodeString(`{
    "data": [
      {"id": "1", "type": "foos", "payload": "zzzz"},
      {"id": "2", "type": "foos", "payload": "xxxx"}
    ]
  }`)
  ->Assert.okOf(
    {
      data: [{id: "1", type_: "foos", payload: "zzzz"}, {id: "2", type_: "foos", payload: "xxxx"}],
    },
    ~message="Decodes correctly",
  )

  Assert.roundtrips(
    {
      data: [{id: "1", type_: "foos", payload: "zzzz"}, {id: "2", type_: "foos", payload: "xxxx"}],
    },
    Codecs.message,
    ~message="Does roundtrip",
  )
})
