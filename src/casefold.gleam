import gleam/list
import gleam/string

/// ASCII lowercase letters.
pub const ascii_lowercase = "abcdefghijklmnopqrstuvwxyz"

/// ASCII uppercase letters.
pub const ascii_uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

/// ASCII letters (lower and uppercase).
pub const ascii_letters = ascii_lowercase <> ascii_uppercase

/// The string "0123456789".
pub const digits = "0123456789"

/// The string "0123456789abcdefABCDEF".
pub const hex_digits = "0123456789abcdefABCDEF"

/// The string "01234567".
pub const oct_digits = "01234567"

/// ASCII characters which are considered punctuation characters.
pub const ascii_punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~."

/// ASCII characters which are considered whitespace.
pub const ascii_whitespace = " \t\n\r\f\u{00000b}"

/// All printable ASCII characters.
pub const ascii_printable = digits
  <> ascii_letters
  <> ascii_punctuation
  <> ascii_whitespace

/// Converts a String to a case-agnostic comparable string.
/// `casefold` is preferred over `string.lowercase` when two
/// strings are to be compared for equality.
@external(erlang, "string", "casefold")
@external(javascript, "./casefold_ffi.js", "casefold")
pub fn casefold(s: String) -> String

/// Expand leading tabs with the given tabstop.
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
    "\t" <> rest ->
      do_expand_tabs_indent(
        rest,
        tabstop,
        col + { tabstop - { col % tabstop } },
      )
    _ -> #(s, col)
  }
}

/// Returns a float between +0.0 and 1.0 representing the
/// Jaro similarity between the given strings. Strings with
/// a higher similarity will score closer to 1.0, with +0.0
/// meaning no similarity and 1.0 meaning an exact match.
@external(erlang, "string", "jaro_similarity")
@external(javascript, "./casefold_ffi.js", "jaro_similarity")
pub fn jaro_similarity(first: String, second: String) -> Float

const alnum = ascii_letters <> digits

/// True if all graphemes in string are ASCII
/// alphanumeric characters.
pub fn is_alnum(s: String) -> Bool {
  list.all(string.to_graphemes(s), fn(ch) { string.contains(alnum, ch) })
}

/// True if all graphemes in string are ASCII
/// letters.
pub fn is_alpha(s: String) -> Bool {
  list.all(string.to_graphemes(s), fn(ch) { string.contains(ascii_letters, ch) })
}

/// True if all graphemes in string are ASCII
/// characters.
pub fn is_ascii(s: String) -> Bool {
  list.all(string.to_utf_codepoints(s), fn(cp) {
    let i = string.utf_codepoint_to_int(cp)
    i >= 0 && i < 255
  })
}

/// True if the string contains only spaces and/or tabs
/// or if the string is empty.
pub fn is_hspaces(s: String) -> Bool {
  case s {
    " " <> s | "\t" <> s -> is_hspaces(s)
    "" -> True
    _ -> False
  }
}

/// True if the string contains only ASCII whitespace
/// or if the string is empty.
pub fn is_blank(s: String) -> Bool {
  case s {
    " " <> s | "\t" <> s | "\n" <> s | "\r" <> s | "\f" <> s | "\u{00000b}" <> s ->
      is_blank(s)
    "" -> True
    _ -> False
  }
}

/// Splits the input string into a list of
/// lines with the line endings (\n or \r\n)
/// removed.
/// An empty string splits into an empty list.
@external(erlang, "casefold_ffi", "split_lines")
@external(javascript, "./casefold_ffi.js", "split_lines")
pub fn split_lines(s: String) -> List(String)

