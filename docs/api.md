# Jzon API Reference

## module Jzon

### Types

```rescript
type codec<'v>
```

Defines a thing that can encode a value of type `'v` to `Js.Json.t` and decode it back. You either use one of built-in codecs, compose own codec from others using adaptors, or define a custom one.

```rescript
type field<'v>
```

Defines a field descriptor used to encode and decode an object field of type `'v`. Used for describing object codecs.

### Decoding

```rescript
let decode: (codec<'v>, Js.Json.t) => result<'v, DecodingError.t>
let decodeWith: (Js.Json.t, codec<'v>) => result<'v, DecodingError.t>
let decodeString: (codec<'v>, string) => result<'v, DecodingError.t>
let decodeStringWith: (string, codec<'v>) => result<'v, DecodingError.t>
```

Decode payload given a codec. The functions never throw as long as custom codecs and object constructors do not throw.

If you are in doubt which argument order to prefer, use data-first (i.e. `decodeWith`) for the sake of consistency. Usage experience shows it fits long pipe `->` chains `->` nicely.

### Encoding

```rescript
let encode: (codec<'v>, 'v) => Js.Json.t
let encodeWith: ('v, codec<'v>) => Js.Json.t
let encodeString: (codec<'v>, 'v) => string
let encodeStringWith: ('v, codec<'v>) => string
```

Encodes a value with the given codec. The functions always succeed as long as custom codecs and object destructors do not throw.

If you are in doubt which argument order to prefer, use data-first (i.e. `encodeWith`) for the sake of consistency. Usage experience shows it fits long pipe `->` chains `->` nicely.

### Simple codecs

```rescript
let string: codec<string>
let float: codec<float>
let int: codec<int>
let bool: codec<bool>
let json: codec<Js.Json.t>
```

Provide codecs for scalar JSON types.

The `json` codec is effectively an identity codec that encodes `Js.Json.t` to itself and back without changes. Useful in cases when composing a custom codec for data having different schemas in different cases.

The `int` codec is somewhat opinionated because the JSON standard does not define an integer value, only floating point numbers. The `int` encoding always succeeds but decoding fails with `#UnexpectedJsonValue` in several cases:

- The value contains fractional part (42 and 42.0 are OK, 42.5 is not)
- The value is out of -2^31..+2^31 range and thus cannot be represented with ReScript’s int

### Array adapter

```rescript
let array: codec<'v> => codec<array<'v>>
```

Using the given codec, makes a new codec for an array which uses that given codec to encode and decode array elements.

### Dict adapter

```rescript
let dict: codec<'v> => codec<Js.Dict.t<'v>>
```

Using the given codec, makes a new codec for a dictionary. The dictionary uses that given codec to encode and decode the values. Keys are encoded and decoded as strings without any changes.

### Null adapters

```rescript
let nullable: codec<'v> => codec<option<'v>>
```

Makes a codec accept the `null` JSON value by making its payload type `option`’al. `None` is encoded as JSON `null`, `Some(value)` is encoded as plain `value` would be encoded.

```rescript
let nullAs: (codec<'v>, 'v) => codec<'v>
```

Makes a codec accept the `null` JSON value while decoding by falling back to a predefined default value. The encoding process always outputs the real value even if it is equal to the default value. That is, `nullAs` never _produces_ a JSON `null` value.

### Object codecs

```rescript
// NOTE: Not a real syntax. On practice use object1, object3, object13, etc.,
// depending on the number of fields
let objectN: (
  'r => ('f1, 'f2, ..., 'fN),
  (('f1, 'f2, ..., 'fN)) => result<'r, DecodingError.t>,
  field<'f1>,
  field<'f2>,
  ...
  field<'fN>,
) => codec<'r>
```

Makes an object codec that translates between a JSON object and a ReScript object (record, tuple, custom opaque, whatever). The function takes arguments in the following order:

- _destructor_: a function which takes the ReScript object and converts it to the tuple with the order and element types dictated by the field descriptors
- _constructor_: a function which takes a tuple with the order and element types dictated by the field descriptors, and converts the tuple to the ReScript object. The function may return an `Error(DecodingError.t)` to fail the whole decoding process.
- _N field descriptors_: N arguments which define the object keys and values. The codecs are currently available for N from 1 to 25. If you have an object with more fields, it is likely you have some patterns in it that are better served by a custom codec.

```rescript
let field: (string, codec<'v>) => field<'v>
```

Makes a descriptor for an object field with the given key/name, using the codec provided.

```rescript
let self: field<Js.Json.t>
```

Makes a descriptor referring to the whole object being processed. Useful if the object shape depends on some factors, like a variant value.

```rescript
let optional: field<'v> => field<option<'v>>
```

Makes the given field optional. The field will be decoded to the `None` value if the given field key is missing in the JSON object _or_ if the key is there but its value is `null`. While encoding, if the ReScript value is `None` the given field key will be omitted from the resulting JSON.

```rescript
let default: (field<'v>, 'v) => field<'v>
```

Makes the given field optional. The field will be decoded to the default fallback value provided if the given field key is missing in the JSON object _or_ if the key is there but its value is `null`. While encoding, the resulting JSON always includes the key, even if the value is equal to the default.

### Custom codecs

```rescript
let custom: ('v => Js.Json.t, Js.Json.t => result<'v, DecodingError.t>) => codec<'v>
```

Creates a custom codec. Use this to integrate with existing codebase which does not relies on Jzon or when you have a quirky JSON representation which is hard or impossible to express by composing other codecs and adapters.

The first argument is a function to encode the ReScript value into JSON. And the second argument is a function to decode JSON into the ReScript value.

### Decoding utilities

```rescript
let asObject: Js.Json.t => result<Js.Dict.t<Js.Json.t>, DecodingError.t>
```

Reinterprets a JSON object as a dictionary of JSON objects. Returns `Error(#UnexpectedJsonType(...))` if the argument is not an object (e.g., it is number or null).

## module Jzon.DecodingError

```rescript
type locationComponent = Field(string) | Index(int)

type location = array<locationComponent>

type t = [
  | #SyntaxError(string)
  | #MissingField(location, string /* key */)
  | #UnexpectedJsonType(location, string /* expected */, Js.Json.t /* actual */)
  | #UnexpectedJsonValue(location, string /* actual */)
]
```

An error type that used extensively as a failed `result` payload while decoding.

```rescript
let toString: t => string
```

Formats the error as a string suitable for logging or showing to a user.

```rescript
let prependLocation: (t, locationComponent) => t
```

Transforms the error by prepending a location component. Other details are kept intact.

Use this function to rectify the error location while implementing custom codecs. Particularly, when you drill-down a `Js.Json.t` object manually and mix this with decoding through other codecs which know nothing about your drill-down.
