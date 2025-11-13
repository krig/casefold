import gleam/string

/// ASCII lowercase letters
pub const ascii_lowercase = "abcdefghijklmnopqrstuvwxyz"
/// ASCII uppercase letters
pub const ascii_uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
/// ASCII letters (lower and uppercase)
pub const ascii_letters = ascii_lowercase <> ascii_uppercase
/// The string "0123456789"
pub const digits = "0123456789"
/// The string "0123456789abcdefABCDEF"
pub const hex_digits = "0123456789abcdefABCDEF"
/// The string "01234567"
pub const oct_digits = "01234567"
/// ASCII characters which are considered punctuation characters
pub const ascii_punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~."
/// ASCII characters which are considered whitespace
pub const ascii_whitespace = " \t\n\r\f\u{00000b}"
pub const ascii_printable = digits <> ascii_letters <> ascii_punctuation <> ascii_whitespace

/// Converts a String to a case-agnostic comparable string.
/// `casefold` is preferred over `string.lowercase` when two
/// strings are to be compared for equality.
@external(erlang, "string", "casefold")
@external(javascript, "./casefold_ffi.js", "casefold")
pub fn casefold(s: String) -> String

/// Expand leading tabs with the given tabstop
pub fn expand_tabs(s: String, tabstop: Int) -> String {
  assert tabstop > 0
  let #(body, indent) = do_expand_tabs_indent(s, tabstop, 0)
  case indent {
    0 -> body
    indent -> string.repeat(" ", indent) <> body
  }
}

fn do_expand_tabs_indent(s: String, tabstop: Int, col: Int) -> #(String, Int) {
  case s {
    "    " <> rest -> do_expand_tabs_indent(rest, tabstop, col + 4)
    "   " <> rest -> do_expand_tabs_indent(rest, tabstop, col + 3)
    "  " <> rest -> do_expand_tabs_indent(rest, tabstop, col + 2)
    " " <> rest -> do_expand_tabs_indent(rest, tabstop, col + 1)
    "\t" <> rest -> do_expand_tabs_indent(rest, tabstop, col + { tabstop - { col % tabstop } })
    _ -> #(s, col)
  }
}
