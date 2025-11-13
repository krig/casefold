import gleam/float
import gleeunit
import casefold.{casefold, expand_tabs, jaro_similarity}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn casefold_test() {
  assert casefold("Ω and ẞ SHARP S") == "ω and ss sharp s"
  assert casefold("s") == "s"
  assert expand_tabs("\t", 4) == "    "
  assert expand_tabs(" \t", 2) == "  "
  assert float.loosely_equals(jaro_similarity("FAREMVIEL", "FARMVILLE"), 0.88, 0.01)
}
