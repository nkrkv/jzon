open Test

test("Scalar types", () => {
  Assert.roundtrips("Hello", Jzon.string, ~message="string does roundtrip")
  Assert.roundtrips(42.5, Jzon.float, ~message="float does roundtrip")
  Assert.roundtrips(42, Jzon.int, ~message="int does roundtrip")
  Assert.roundtrips(true, Jzon.bool, ~message="bool does roundtrip")
  Assert.roundtrips(Js.Json.number(42.), Jzon.json, ~message="Raw JSON does roundtrip")
})

test("Int codec", () => {
  Jzon.int
  ->Jzon.decodeString("42.5")
  ->Assert.errorString("Unexpected value 42.5 at .", ~message="Barks on fractional numbers")

  Jzon.int
  ->Jzon.decodeString("9111222333")
  ->Assert.errorString("Unexpected value 9111222333 at .", ~message="Barks on out-of-range numbers")
})

test("Nullable", () => {
  Assert.roundtrips(
    Some("Hello"),
    Jzon.nullable(Jzon.string),
    ~message="Some(string) does roundtrip",
  )

  Assert.roundtrips(None, Jzon.nullable(Jzon.string), ~message="None does roundtrip")
  Jzon.nullable(Jzon.string)
  ->Jzon.encode(None)
  ->Assert.equals(Js.Json.null, ~message="Encodes as `null`")
})

test("Null as", () => {
  Jzon.int
  ->Jzon.nullAs(100)
  ->Jzon.decodeString("null")
  ->Assert.okOf(100, ~message="Decodes null as value provided")
})

test("Array", () => {
  Assert.roundtrips(
    [4, 8, 15, 16, 23, 42],
    Jzon.array(Jzon.int),
    ~message="array<int> does roundtrip",
  )

  Jzon.array(Jzon.int)
  ->Jzon.decodeString(`[1, 2, "three", 4]`)
  ->Assert.errorString(
    "Expected number, got string at .[2]",
    ~message="Barks on unexpected type with proper path",
  )

  Jzon.array(Jzon.nullable(Jzon.int))
  ->Jzon.decodeString(`[1, 2, null, 4]`)
  ->Assert.okOf([Some(1), Some(2), None, Some(4)], ~message="Handles nullables")
})

test("JSON with syntax error", () => {
  Jzon.json
  ->Jzon.decodeString(`{"color": "#09a", size: 5.0}`)
  ->Assert.errorString("Unexpected token s in JSON at position 18", ~message="Errors")
})
