import gleeunit
import casefold.{casefold}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn casefold_test() {
  assert casefold("Ω and ẞ SHARP S") == "ω and ss sharp s"
  assert casefold("s") == "s"
}
