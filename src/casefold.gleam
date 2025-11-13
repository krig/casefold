/// Converts a String to a case-agnostic comparable string.
/// `casefold` is preferred over `string.lowercase` when two
/// strings are to be compared for equality.
@external(erlang, "string", "casefold")
@external(javascript, "./casefold_ffi.js", "casefold")
pub fn casefold(s: String) -> String
