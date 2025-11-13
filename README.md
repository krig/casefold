# casefold

[![Package Version](https://img.shields.io/hexpm/v/casefold)](https://hex.pm/packages/casefold)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/casefold/)

Converts a String to a case-agnostic comparable string.
`casefold` is preferred over `string.lowercase` when two
strings are to be compared for equality.

```sh
gleam add casefold@1
```
```gleam
import casefold.{casefold}

pub fn main() -> Nil {
  assert casefold("Ω and ẞ SHARP S") == "ω and ss sharp s"
}
```

Further documentation can be found at <https://hexdocs.pm/casefold>.

## Development

```sh
gleam build # Build the project
gleam test  # Run the tests
```
