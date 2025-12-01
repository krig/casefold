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

/// Splits the input string into a list of
/// words.
/// An empty string splits into an empty list.
@external(erlang, "casefold_ffi", "split_words")
@external(javascript, "./casefold_ffi.js", "split_words")
pub fn split_words(s: String) -> List(String)

/// Format string
///
/// See also: `format_named`
///
/// Provides a basic string formatter.
/// Does not handle type conversion, all
/// values provided must be strings.
///
/// Values are inserted at {} slots in
/// the format string. To include a literal
/// { or }, use {{ or }}.
///
///
/// Example:
///
/// ```gleam
/// import gleam/io
/// import casefold.{format}
///
/// pub fn main() {
///   io.println(format("Hello, {}!", ["Joe"]))
/// }
/// ```
pub fn format(fmt: String, slots: List(String)) -> String {
  let ptn = format_compile()
  format_acc(ptn, fmt, slots, "")
}

/// Format string (key/value)
///
/// Provides a basic string formatter.
/// Does not handle type conversion, all
/// values provided must be strings.
///
/// Values are inserted at {key} slots in
/// the format string. To include a literal
/// { or }, use {{ or }}.
///
///
/// Example:
///
/// ```gleam
/// import gleam/io
/// import casefold.{format_named}
///
/// pub fn main() {
///   io.println(format_named("Hello, {name}!", [#("name", "Joe")]))
/// }
/// ```
pub fn format_named(fmt: String, slots: List(#(String, String))) -> String {
  let ptn = format_compile()
  format_named_acc(ptn, fmt, slots, "")
}

fn format_acc(ptn, fmt, slots, acc) {
  let #(acc, s, r) = format_split(ptn, fmt, acc)
  case s {
    "" -> acc
    "{" ->
      case r {
        "}" <> r ->
          case slots {
            [v, ..slots] -> format_acc(ptn, r, slots, acc <> v)
            _ ->
              panic as {
                "format_acc:"
                <> string.inspect(ptn)
                <> " "
                <> fmt
                <> " "
                <> string.inspect(slots)
                <> " "
                <> acc
              }
          }
        "{" <> r -> format_acc(ptn, r, slots, acc <> "{")
        _ -> panic as "unmatched {"
      }
    "}" ->
      case r {
        "}" <> r -> format_acc(ptn, r, slots, acc <> "}")
        _ -> panic as "unmatched }"
      }
    _ -> panic as "Unreachable"
  }
}

fn format_named_acc(ptn, fmt, slots, acc) {
  let #(acc, s, r) = format_split(ptn, fmt, acc)
  case s {
    "" -> acc
    "{" -> {
      let #(key, s2, r) = format_split(ptn, r, "")
      case s2 {
        "}" ->
          case list.key_find(slots, key) {
            Ok(v) -> format_named_acc(ptn, r, slots, acc <> v)
            Error(_) -> panic as { "No value provided for key=" <> key }
          }
        _ -> panic as "Unmatched {"
      }
    }
    "}" ->
      case r {
        "}" <> r -> format_named_acc(ptn, r, slots, acc <> "}")
        _ -> panic as "unmatched }"
      }
    _ -> panic as "Unreachable"
  }
}

type Pattern

@external(erlang, "casefold_ffi", "format_compile")
@external(javascript, "./casefold_ffi.js", "formatCompile")
fn format_compile() -> Pattern

@external(erlang, "casefold_ffi", "format_split")
@external(javascript, "./casefold_ffi.js", "formatSplit")
fn format_split(
  _ptn: Pattern,
  _fmt: String,
  _acc: String,
) -> #(String, String, String)
