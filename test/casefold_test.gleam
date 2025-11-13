import casefold.{casefold, expand_tabs, jaro_similarity, split_lines}
import gleam/float
import gleeunit

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn casefold_test() {
  assert casefold("Ω and ẞ SHARP S") == "ω and ss sharp s"
  assert casefold("s") == "s"
  assert expand_tabs("\t", 4) == "    "
  assert expand_tabs(" \t", 2) == "  "
  assert float.loosely_equals(
    jaro_similarity("FAREMVIEL", "FARMVILLE"),
    0.88,
    0.01,
  )
  assert float.loosely_equals(jaro_similarity("ditto", "ditto"), 1.0, 0.01)
  assert float.loosely_equals(jaro_similarity("foo", "bar"), 0.0, 0.01)
  assert float.loosely_equals(
    jaro_similarity("michelle", "michael"),
    0.87,
    0.01,
  )
  assert float.loosely_equals(jaro_similarity("Édouard", "Claude"), 0.53, 0.01)
  assert casefold.is_blank(" foo") == False
  assert casefold.is_blank(" \t") == True
  assert casefold.is_hspaces(" \t") == True
  assert casefold.is_hspaces(" \t\n") == False


  assert equal_lists(split_lines(""), [])
  assert equal_lists(split_lines("\n"), ["", ""])
  assert equal_lists(split_lines("a\nb"), ["a", "b"])
}

fn equal_lists(a: List(String), b: List(String)) -> Bool {
  case a, b {
    [av, ..arest], [bv, ..brest] -> case av == bv {
      True -> equal_lists(arest, brest)
      False -> False
    }
    [], [] -> True
    _, _ -> False
  }
}
