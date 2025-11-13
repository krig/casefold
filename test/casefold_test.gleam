import gleeunit
import casefold.{casefold, expand_tabs}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn casefold_test() {
  assert casefold("Ω and ẞ SHARP S") == "ω and ss sharp s"
  assert casefold("s") == "s"
  assert expand_tabs("\t", 4) == "    "
  assert expand_tabs(" \t", 2) == "  "
}
