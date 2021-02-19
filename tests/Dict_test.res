let test = (name, fn) => Test.test(`Dict » ` ++ name, fn)

test("Roundtrip", () => {
  Assert.roundtrips(
    Js.Dict.fromArray([("one", 1), ("two", 2), ("three", 3)]),
    Jzon.dict(Jzon.int),
    ~message="succeeds",
  )
})

test("Decoding", () => {
  Jzon.dict(Jzon.int)
  ->Jzon.decodeString(`{
      "one": 1,
      "two": 2,
      "three": "борщ"
    }`)
  ->Assert.errorString(
    `Expected number, got string at ."three"`,
    ~message="errors on wrong value type",
  )
})
