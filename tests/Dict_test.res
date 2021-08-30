let test = (name, fn) => Test.test(`Dict » ` ++ name, fn)

test("Roundtrip", () => {
  Assert.roundtrips(
    Js.Dict.fromArray([("one", 1), ("two", 2), ("three", 3)]),
    Jzon.dict(Jzon.int),
    ~message="succeeds",
  )
})

test("Decoding", () => {
  `{
    "one": 1,
    "two": 2,
    "three": "борщ"
  }`
  ->Jzon.decodeStringWith(Jzon.dict(Jzon.int))
  ->Assert.errorString(
    `Expected number, got string at ."three"`,
    ~message="errors on wrong value type",
  )
})
