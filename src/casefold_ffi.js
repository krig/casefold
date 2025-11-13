export function casefold(s) {
  return s.toLowerCase().toUpperCase().toLowerCase();
}

function matches(max_distance, s1, s2) {
  let matches = "";
  let counted = new Array(s2.length);
  for (let i = 0; i < s1.length; i++) {
    let lo = Math.max(i - max_distance, 0);
    let hi = Math.min(i + max_distance, s2.length);
    for (let j = lo; j <= hi; j++) {
      if (s1[i] === s2[j] && counted[j] !== true) {
        counted[j] = true;
        matches += s1[i];
        break;
      }
    }
  }
  return matches;
}

function transpositions(m1, m2) {
  let transpositions = 0;
  for (let i = 0; i < Math.min(m1.length, m2.length); i++) {
    transpositions += m1[i] !== m2[i] ? 1 : 0
  }
  return Math.floor(transpositions / 2);
}

export function jaro_similarity(first, second) {
  const max_distance = Math.floor(Math.max(first.length, second.length)/2) - 1;
  let m1 = matches(max_distance, first, second);
  let m = m1.length;
  if (m < 1) return 0.0;
  const m2 = matches(max_distance, second, first);
  let t = transpositions(m1, m2);
  return ((m/first.length) + (m/second.length) + (m - t)/m)/3;
}
