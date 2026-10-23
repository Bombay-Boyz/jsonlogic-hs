#!/usr/bin/env node
// Outlier fixture generator.
//
// Builds a large, fully deterministic catalogue of awkward rules and data
// (no randomness: the same script always writes the same bytes), runs every
// case through the reference implementation (json-logic-js, pinned in
// package.json), and records the reference's answer.
//
// Usage:  node outliers.js OUTPUT.jsonl        (writes the fixture)
//         node outliers.js --check FIXTURE     (fails if FIXTURE is stale)
//
// The Haskell test-suite `jsonlogic-hs-outliers` replays the fixture
// offline, so running it needs neither Node nor network access. Regenerate
// the fixture only when this file changes; CI runs --check so the committed
// fixture can never drift from the generator.
//
// The first line is {"meta":{"cases":N,"oracle":"2.0.5","tier":"core"}}, so a
// truncated fixture is detected. Every other line is one case:
//   {"id":"...","fam":"...","op":"...","n":2,"rule":R,"data":D,"expect":E}
// where E is {"ok":value} or {"error":"message"}. JavaScript `undefined`,
// NaN and the infinities are written as null (JSON.stringify), and lone
// surrogates in output are normalised to U+FFFD (toWellFormed), because a
// Haskell Text cannot hold them. `n` is the number of operands written, so
// the runner can tell strict-arity rejections from real disagreements.

"use strict";
const fs = require("fs");
const jsonLogic = require("json-logic-js");

const REQUIRED_ORACLE = "2.0.5";
const version = require("json-logic-js/package.json").version;
if (version !== REQUIRED_ORACLE) {
  console.error(`oracle is json-logic-js ${version}, need ${REQUIRED_ORACLE}`);
  process.exit(2);
}

// ---------------------------------------------------------------------
// Serialisation that can carry raw JSON tokens (1e309, -0 ...) which
// JSON.stringify would destroy.
// ---------------------------------------------------------------------
const raw = (text) => ({ __raw: text });
function ser(x) {
  if (x !== null && typeof x === "object" && "__raw" in x) return x.__raw;
  if (Array.isArray(x)) return "[" + x.map(ser).join(",") + "]";
  if (x !== null && typeof x === "object") {
    return "{" + Object.keys(x).map((k) => JSON.stringify(k) + ":" + ser(x[k])).join(",") + "}";
  }
  return JSON.stringify(x);
}
const op = (name, ...args) => ({ [name]: args });
const opRaw = (name, args) => ({ [name]: args });

// ---------------------------------------------------------------------
// Value pools
// ---------------------------------------------------------------------
const NEG_ZERO = op("*", -1, 0);
const POS_INF = op("/", 1, 0);
const NEG_INF = op("/", -1, 0);
const NOT_A_NUMBER = op("+", "a");
const UNDEF = op("and");

const NUMBERS = [
  0, 1, -1, 2, 3, 7, 0.5, -0.5, 2.5, -2.5, 3.5, 10, 100, 1e21, 1e-7, 1e-6,
  123456789012345680000, 5e-324, 2.2250738585072014e-308, 1.7976931348623157e308,
  raw("1e309"), raw("-1e309"), raw("1e-400"), raw("-1e-400"), raw("1E5"), raw("1.0"), raw("-0.0"),
  raw("0e0"), raw("100e-2"), raw("9007199254740991"), raw("9007199254740992"),
  raw("9007199254740993"), raw("9007199254740995"), raw("18014398509481984"),
  0.1, 0.2, 0.30000000000000004, 4294967296, 4294967295, 2147483648, -2147483649, 1e300,
  raw("123456789012345678901234567890"), raw("0.000001"), raw("0.0000001"),
  raw("1e21"), raw("1e22"), raw("999999999999999900000"), raw("1.7976931348623158e308"),
  raw("2.2250738585072011e-308"), raw("4.9406564584124654e-324"), raw("2e-324"), raw("3e-324"),
];
const COMPUTED_NUMBERS = [NEG_ZERO, POS_INF, NEG_INF, NOT_A_NUMBER, op("-", 0)];

// Strings that stress ToNumber / parseFloat.
const WS = [
  "\u0009", "\u000a", "\u000b", "\u000c", "\u000d", "\u0020", "\u00a0", "\u1680",
  "\u2000", "\u2001", "\u2002", "\u2003", "\u2004", "\u2005", "\u2006", "\u2007",
  "\u2008", "\u2009", "\u200a", "\u2028", "\u2029", "\u202f", "\u205f", "\u3000", "\ufeff",
];
const NOT_WS = ["\u0085", "\u180e", "\u200b", "\u001c", "\u001f", "\u0000", "\u200c", "\u2060", "\ufffe"];
const NUMERIC_STRINGS = [
  "", " ", "0", "1", "-1", "+1", "00", "01", "-0", "+0", "0.0", "0x10", "0X1F", "0xg", "0x",
  "0b101", "0B11", "0b2", "0b", "0o17", "0O7", "0o8", "0o", "-0x10", "+0x10", "0x-1", "0x1.8",
  "1_000", "1e3", "1E3", "1e", "1e+", "1e-", "1e+3", "1e-3", "1e3.5", ".5", "5.", ".", "-.",
  "+.5e-3", "-.5", ".e1", "1..2", "1.2.3", "1,5", "Infinity", "-Infinity", "+Infinity",
  "infinity", "INFINITY", "Infinityx", "Infinit", "NaN", "nan", "-NaN", " 12 ", "\t12\n",
  "12px", "12 13", "1 2", "١٢٣", "１２３", "²", "①", "१२३", "a", "abc", "true", "false", "null",
  "undefined", "[object Object]", "[1]", "1,2", "1e1000", "-1e1000", "1e-1000", "0e5", "0.0e5",
  "9007199254740993", "9007199254740992.5", "0.1", "0.30000000000000004",
  "1.7976931348623157e308", "1.7976931348623159e308", "1.797693134862315807e308",
  "4.9e-324", "2e-324", "2.4703282292062327e-324", "2.4703282292062328e-324", "5e-324",
  "2.2250738585072014e-308", "2.225073858507201e-308", "123456789012345678901234567890",
  "0.000001", "0.0000001", "1e21", "1e-7", "-1e-7", "+-1", "--1", "- 1", "+ 1", "1 ", " 1",
  "\u00001", "1\u0000", "0.1e1", "00.5", "0001e3", "1e0003", "1e-0003", ".0e0", "-.0",
  "1".repeat(20), "1".repeat(400), "1".repeat(767), "1".repeat(768), "1".repeat(769),
  "9".repeat(309), "9".repeat(310), "0." + "0".repeat(323) + "1", "0." + "0".repeat(400) + "1",
  "1" + "0".repeat(308), "1" + "0".repeat(309),
  "0." + "0".repeat(20) + "9".repeat(30), "2.4703282292062327" + "0".repeat(300) + "1e-324",
  "5e-325", "2.5e-324", "1.5e-323",
];
for (const w of WS) {
  NUMERIC_STRINGS.push(w + "12", "12" + w, w + "12" + w, w, w + "0x10", w + "-5" + w);
}
for (const w of NOT_WS) {
  NUMERIC_STRINGS.push(w + "12", "12" + w);
}

// Strings that stress length / indexing / ordering / substr.
const UNICODE_STRINGS = [
  "", "a", "ab", "abc", "z", "A", "é", "e\u0301", "\u0000", "\u0000a", "a\u0000b", "😀", "a😀b",
  "😀😀", "👨‍👩‍👧", "🇮🇳", "🇮🇳🇺🇸", "👍🏽", "\ue000", "\uffff", "\ufffd", "\ud7ff", "\uf8ff",
  "\u{10000}", "\u{10ffff}", "\u{1d11e}", "𝒳", "שלום", "ab\u202ec", "\ud83d\ude00", "ǆ", "ß", "İ", "日本語", "\u{20bb7}",
];

const CONTAINERS = [
  [], [[]], [0], [1], [null], [[1]], [1, 2], [1, 2, 3], ["a"], [[], []], {}, { a: 1 },
  { a: {} }, [{}], [true], ["1"], [false], [""], [0, 0], [[], 0], ["a", "b"], [1, "1"],
  { a: [1, 2] }, [null, null],
];
const MISC = [null, true, false, UNDEF];

// A rich pool for unary experiments and a smaller one for pairs / triples.
const POOL_A = [
  ...NUMBERS, ...COMPUTED_NUMBERS, ...MISC, ...CONTAINERS,
  ...[
    "", " ", "0", "1", "-1", "a", "abc", "0x10", "1e3", " 12 ", "Infinity", "-Infinity", "NaN",
    ".5", "5.", "true", "null", "undefined", "1,2", "[object Object]", "\ufeff1", "\u2028 1",
    "\u00a01", "\u00851", "\u180e1", "\u200b1", "😀", "e\u0301", "\u0000", "١٢٣",
  ],
];
const POOL_B = [
  0, 1, -1, 0.5, 1e21, 1e-7, 5e-324, 1.7976931348623157e308, raw("9007199254740993"),
  NEG_ZERO, POS_INF, NEG_INF, NOT_A_NUMBER, null, true, false, UNDEF,
  "", "0", "1", "a", "10", "9", "0x10", " 12 ", "Infinity", "NaN", "😀", "\ue000",
  [], [0], [1], [1, 2], ["a"], {}, { a: 1 },
];
const POOL_C = [0, 1, "a", "", null, UNDEF, [1, 2], NEG_ZERO];

// ---------------------------------------------------------------------
// Case collection
// ---------------------------------------------------------------------
const cases = [];
const seen = new Set();
let counter = 0;

function add(fam, opName, n, rule, data) {
  const ruleText = ser(rule);
  const dataText = ser(data === undefined ? null : data);
  const key = ruleText + "\u0001" + dataText;
  if (seen.has(key)) return;
  seen.add(key);
  counter += 1;
  cases.push({ id: fam + "-" + String(counter).padStart(6, "0"), fam, op: opName, n, ruleText, dataText });
}

const UNARY_OPS = [
  "var", "missing", "!", "!!", "-", "+", "*", "max", "min", "cat", "log", "merge", "if", "?:",
  "and", "or", "==", "===", "!=", "!==", "<", "<=", ">", ">=", "/", "%", "in", "substr", "map",
  "filter", "reduce", "all", "none", "some", "missing_some",
];
const PAIR_OPS = [
  "==", "===", "!=", "!==", "<", "<=", ">", ">=", "+", "-", "*", "/", "%", "max", "min", "cat",
  "in", "merge", "substr", "var", "missing", "missing_some", "and", "or", "if", "map", "filter",
  "all", "none", "some", "reduce",
];
const TRIPLE_OPS = ["<", "<=", "substr", "if", "and", "or", "reduce", "cat", "+", "max", "*", "var", "-"];
// Every builtin operator. It must be the set in src/JsonLogic/Core/Operator.hs:
// the Haskell runner fails if any builtin there has too few cases here.
const ALL_OPS = [
  "var", "missing", "missing_some", "if", "?:", "and", "or", "!", "!!", "==", "===", "!=", "!==",
  "<", "<=", ">", ">=", "max", "min", "+", "-", "*", "/", "%", "map", "filter", "reduce", "all",
  "none", "some", "merge", "in", "cat", "substr", "log",
];

// 1. Every operator at arities 0..4 with simple operands.
const ARITY_OPERANDS = [1, "a", [1, 2], null];
for (const name of ALL_OPS) {
  for (let n = 0; n <= 4; n++) {
    add("arity", name, n, opRaw(name, ARITY_OPERANDS.slice(0, n)), { a: 5 });
    add("arity", name, n, opRaw(name, [{ var: "a" }, { var: "a" }, { var: "a" }, { var: "a" }].slice(0, n)), { a: 5 });
    add("arity", name, n, opRaw(name, [[1, 2, 3], { "+": [{ var: "" }, 1] }, 0, "x"].slice(0, n)), { a: 5 });
  }
}
// bare (non-array) operand shorthand
for (const name of ALL_OPS) for (const v of [1, "a", null, true, { var: "a" }]) {
  add("arity", name, 1, { [name]: v }, { a: 5 });
}

// 2. Unary experiments over the rich pool.
for (const name of UNARY_OPS) for (const v of POOL_A) add("unary", name, 1, opRaw(name, [v]), null);

// 3. Pair experiments.
for (const name of PAIR_OPS) {
  for (const a of POOL_B) for (const b of POOL_B) add("pair", name, 2, opRaw(name, [a, b]), { a: 1, b: [1, 2] });
}

// 4. Triple experiments.
for (const name of TRIPLE_OPS) {
  for (const a of POOL_C) for (const b of POOL_C) for (const c of POOL_C) add("triple", name, 3, opRaw(name, [a, b, c]), null);
}

// 5. Number-string conversion through many operators (also via data).
const NUM_CONTEXTS = [
  (t) => op("+", t), (t) => op("*", t, 1), (t) => op("-", t), (t) => op("-", t, 0),
  (t) => op("<", t, 1), (t) => op("==", t, 0), (t) => op("/", t, 1), (t) => op("%", t, 7),
  (t) => op("max", t), (t) => op("min", t, 5), (t) => op("cat", op("+", t)),
  (t) => op("+", { var: "s" }, t), (t) => op("===", op("*", t, 1), 0),
  (t) => op("!!", t), (t) => op(">", t, -1),
];
for (const s of NUMERIC_STRINGS) {
  NUM_CONTEXTS.forEach((ctx, i) => add("numstr", "numeric", 1, ctx(s), { s }));
}

// 6. Number printing through cat and through results.
const PRINT_NUMBERS = [...NUMBERS, ...COMPUTED_NUMBERS];
for (const v of PRINT_NUMBERS) {
  add("numfmt", "cat", 1, op("cat", v), null);
  add("numfmt", "+", 1, op("+", v), null);
  add("numfmt", "*", 2, op("*", v, 1), null);
  add("numfmt", "-", 2, op("-", v, 0), null);
  add("numfmt", "*", 2, op("*", v, 3), null);
  add("numfmt", "/", 2, op("/", v, 3), null);
  add("numfmt", "/", 2, op("/", 3, v), null);
  add("numfmt", "cat", 2, op("cat", "n=", v), null);
  add("numfmt", "in", 2, op("in", v, [v]), null);
}
for (const [a, b] of [[0.1, 0.2], [0.1, 0.7], [1e21, 1], [9007199254740992, 1], [9007199254740993, 2],
  [raw("1e308"), raw("1e308")], [5e-324, 5e-324], [5e-324, 0.5], [0.3, -0.1], [1e16, 1], [1.5, 1.5]]) {
  for (const o of ["+", "-", "*", "/", "%", "max", "min"]) add("numfmt", o, 2, op(o, a, b), null);
}

// 6b. Shortest-digit ties: doubles exactly halfway between two decimals of
// the shortest length (ECMAScript picks the even one).
for (let k = 0n; k < 60n; k++) {
  const big = String(9007199254740992n + k);
  for (const divisor of [10, 100, 1000, 8, 3, 7]) add("tie", "/", 2, op("/", raw(big), divisor), null);
  add("tie", "*", 2, op("*", raw(big), 1.1), null);
  add("tie", "+", 2, op("+", raw(big), 0.5), null);
  add("tie", "-", 2, op("-", raw(String(4503599627370496n + k)), 0.5), null);
  add("tie", "cat", 1, op("cat", op("/", raw(String(18014398509481984n + k * 3n)), 100)), null);
}
for (const x of [0.5, 1.5, 2.5, 4503599627370495.5, 4503599627370496.5, 1125899906842624.25, 562949953421312.125]) {
  for (const d of [1, 10, 100, 3]) add("tie", "/", 2, op("/", x, d), null);
  add("tie", "cat", 1, op("cat", x), null);
}

// 7. Signed zero, NaN, infinity: every combination, then observed.
const SZ = [0, NEG_ZERO, 1, -1, 2, -2, POS_INF, NEG_INF, NOT_A_NUMBER];
const OBSERVERS = [
  (x) => x,
  (x) => op("/", 1, x),
  (x) => op("/", -1, x),
  (x) => op(">", 0, op("/", 1, x)),
  (x) => op("<", 0, op("/", 1, x)),
  (x) => op("===", op("/", 1, x), op("/", 1, 0)),
  (x) => op("===", op("/", 1, x), op("/", -1, 0)),
  (x) => op("cat", x),
  (x) => op("!", x),
  (x) => op("+", x),
];
for (const name of ["max", "min", "%", "*", "/", "+", "-"]) {
  for (const a of SZ) for (const b of SZ) {
    for (const [i, obs] of OBSERVERS.entries()) add("signedzero", name, 2, obs(op(name, a, b)), null);
  }
}
for (const a of SZ) for (const b of SZ) for (const c of SZ) {
  add("signedzero", "max", 3, op("/", 1, op("max", a, b, c)), null);
  add("signedzero", "min", 3, op("/", 1, op("min", a, b, c)), null);
}
// literal -0 in JSON text is read by JSON.parse as -0; aeson reads it as 0.
// That input difference is recorded as a deviation, not tested here.

// 8. Unicode: length, indexing, substr, ordering, in, cat.
const STARTS = [-100, -3, -2, -1, 0, 1, 2, 3, 5, 100, 0.5, 1.9, -1.9, raw("1e21"), NOT_A_NUMBER, null, "1", ""];
const LENS = [undefined, 0, 1, 2, 3, -1, -2, -3, 100, 0.5, 1.9, -0.5, NOT_A_NUMBER, null, "2", POS_INF, NEG_INF];
for (const s of UNICODE_STRINGS) {
  add("unicode", "var", 1, { var: "s.length" }, { s });
  add("unicode", "var", 1, { var: "s" }, { s });
  for (let i = 0; i < 7; i++) add("unicode", "var", 1, { var: "s." + i }, { s });
  add("unicode", "var", 1, { var: "s.7" }, { s });
  add("unicode", "cat", 2, op("cat", s, s), null);
  add("unicode", "in", 2, op("in", s, "x" + s + "y"), null);
  add("unicode", "in", 2, op("in", "", s), null);
  add("unicode", "!", 1, op("!", s), null);
  for (const st of STARTS) {
    add("unicode", "substr", 2, op("substr", s, st), null);
    for (const ln of LENS) {
      if (ln === undefined) continue;
      add("unicode", "substr", 3, op("substr", s, st, ln), null);
    }
  }
}
for (const a of UNICODE_STRINGS) for (const b of UNICODE_STRINGS) {
  for (const o of ["<", "<=", ">", ">=", "==", "===", "!=", "!==", "in", "max", "min"]) add("unicode", o, 2, op(o, a, b), null);
  add("unicode", "<", 3, op("<", a, b, "\uffff"), null);
}
for (const s of UNICODE_STRINGS) {
  add("unicode", "map", 2, op("map", [s], { var: "" }), null);
  add("unicode", "merge", 2, op("merge", [s], s), null);
}

// 9. Variable paths on awkward data.
const PATHS = [
  "", null, "a", "a.b", "a.0", "a.1", "a.length", ".", "a.", ".a", "..", "a..b", "constructor",
  "__proto__", "toString", "valueOf", "hasOwnProperty", "length", "0", "1", "2", "01", "-1", "1e1",
  "4294967296", "4294967295", "4294967294", "1.5", "a.constructor", "a.__proto__", "a.toString",
  "length.length", "isPrototypeOf", "toLocaleString", "map", "concat", "indexOf", "charAt", "trim", "0.0", "0.length", " ", "a b", "A", "α", "😀", "a.😀", "[0]", "a[0]", "a.b.c.d",
  "0.a", raw("0"), raw("1"), raw("1.5"), true, false, [], {}, [0], ["a"], ["a", "b"], 0, 1, -1, 2, raw("1e21"),
];
const VAR_DATA = [
  null, {}, [], "", "str", "😀", "ab😀cd", 0, 5, true, false, [1, 2, 3], [[1], [2]],
  { a: 1 }, { a: null }, { a: "" }, { a: [] }, { a: {} }, { a: [1, { b: 2 }] }, { a: { b: { c: { d: 4 } } } },
  { "": 1 }, { a: { "": 5 } }, { 1: "x", 0: "y" }, { length: 5 }, { constructor: 1 }, { toString: 2 },
  { hasOwnProperty: 3 }, { a: "xyz" }, { a: [10, 20] }, { "a.b": 7 }, { ".": 8 }, { "a": { "b": null } },
  { 0: "z" }, { "01": "z" }, { "-1": "m" }, { "4294967296": "k" }, { "0.0": 1 },
  { a: 1, b: 2 }, { "😀": 1 }, { "α": 2 },
];
VAR_DATA.push(raw('{"__proto__":{"x":1}}'), raw('{"__proto__":1}'));
for (const d of VAR_DATA) for (const p of PATHS) {
  add("var", "var", 1, op("var", p), d);
  add("var", "var", 2, op("var", p, "dflt"), d);
  add("var", "var", 2, op("var", p, null), d);
  add("var", "var", 2, op("var", p, UNDEF), d);
  add("var", "missing", 1, op("missing", p), d);
  add("var", "missing", 1, opRaw("missing", [[p, "a"]]), d);
  add("var", "missing_some", 2, op("missing_some", 1, [p, "a"]), d);
}
for (const d of VAR_DATA) {
  add("var", "missing", 0, op("missing"), d);
  add("var", "missing", 2, op("missing", "a", "b"), d);
  add("var", "missing", 3, op("missing", "a", "b", "c"), d);
  add("var", "missing_some", 2, op("missing_some", 0, ["a", "b"]), d);
  add("var", "missing_some", 2, op("missing_some", 1, ["a", "b"]), d);
  add("var", "missing_some", 2, op("missing_some", 2, ["a", "b"]), d);
  add("var", "missing_some", 2, op("missing_some", 3, ["a", "b"]), d);
  add("var", "missing_some", 2, op("missing_some", "1", ["a", "b"]), d);
  add("var", "missing_some", 2, op("missing_some", null, ["a", "b"]), d);
  add("var", "missing_some", 2, op("missing_some", 1, []), d);
  add("var", "missing_some", 2, op("missing_some", 1, "ab"), d);
  add("var", "missing_some", 2, op("missing_some", 1, null), d);
  add("var", "missing_some", 2, op("missing_some", 1, { a: 1 }), d);
  add("var", "missing", 1, op("missing", op("var", "a")), d);
  add("var", "missing", 1, op("missing", { merge: ["a", ["b"]] }), d);
}

// 10. Collections: every iterating operator over awkward collections and bodies.
const COLLECTIONS = [
  [], [0], [1], [1, 2, 3], [0, 0], [null], [[1], [2]], ["a", "b"], [1, "1", true, null], [{}], [{ a: 1 }, { a: 2 }],
  [[]], "str", "", null, 0, 5, true, {}, { a: 1 }, UNDEF, NOT_A_NUMBER, [UNDEF], [NEG_ZERO, 0],
  { var: "xs" }, { merge: [[1], [2, 3]] }, { var: "o" },
];
const BODIES = [
  { var: "" }, true, false, null, 0, 1, "s", { "+": [{ var: "" }, 1] }, { "!": { var: "" } },
  { ">": [{ var: "" }, 1] }, { var: "a" }, { var: "current" }, { var: "accumulator" },
  { "+": [{ var: "current" }, { var: "accumulator" }] },
  { cat: [{ var: "accumulator" }, { var: "current" }] },
  { merge: [{ var: "accumulator" }, [{ var: "current" }]] },
  { if: [{ var: "" }, 1, 2] }, { nonexistent: [] }, { "==": [{ var: "" }, 1] },
  { var: ["", "dflt"] }, { and: [] }, { missing: ["a"] }, { in: [{ var: "" }, [1, 2]] },
  { "*": [{ var: "current" }, 2] }, { max: [{ var: "current" }, { var: "accumulator" }] },
  { "/": [1, { var: "" }] }, { "%": [{ var: "" }, 2] }, { var: "length" },
];
const COLL_DATA = { xs: [1, 2, 3], o: { a: 1 }, a: 9 };
for (const name of ["map", "filter", "all", "none", "some"]) {
  for (const c of COLLECTIONS) for (const b of BODIES) add("collection", name, 2, op(name, c, b), COLL_DATA);
}
for (const c of COLLECTIONS) for (const b of BODIES) for (const init of [undefined, 0, "", [], null, UNDEF, 1, "x"]) {
  if (init === undefined) add("collection", "reduce", 2, op("reduce", c, b), COLL_DATA);
  else add("collection", "reduce", 3, op("reduce", c, b, init), COLL_DATA);
}
for (const a of COLLECTIONS) for (const b of COLLECTIONS) {
  add("collection", "merge", 2, op("merge", a, b), COLL_DATA);
  add("collection", "in", 2, op("in", a, b), COLL_DATA);
  add("collection", "cat", 2, op("cat", a, b), COLL_DATA);
}

// 11. Control flow, short-circuit, unknown operators in untaken branches.
const BAD = { nonexistent: [] };
// The reference throws on an empty * only when it is evaluated
// ("Reduce of empty array with no initial value"), so in an untaken branch it
// is harmless.
const EMPTY_MUL = { "*": [] };
const FLOW = [
  op("if", true, 1, BAD), op("if", false, BAD, 2), op("if", false, BAD), op("if", true, 1, BAD, BAD),
  op("if", false, 1, true, 2, BAD), op("if", false, 1, false, 2, 3), op("if", false, 1, false, 2),
  op("if", 1), op("if", BAD), op("if", true, BAD), op("if", false, BAD), op("if", [], 1, 2), op("if", [0], 1, 2),
  op("if", "0", 1, 2), op("if", "", 1, 2), op("if", NEG_ZERO, 1, 2), op("if", NOT_A_NUMBER, 1, 2),
  op("if", {}, 1, 2), op("if", UNDEF, 1, 2), op("?:", true, 1, 2), op("?:", false, 1, 2), op("?:", 0, 1),
  op("and", false, BAD), op("and", true, BAD), op("or", true, BAD), op("or", false, BAD),
  op("and", 1, 2, 3), op("and", 1, 0, BAD), op("and", 0), op("and", [], 1), op("and", "", 1),
  op("or", 0, "", [], null), op("or", 0, "", [], 7), op("or", [], {}), op("and", {}, []), op("or"),
  op("and", NEG_ZERO), op("or", NOT_A_NUMBER, 0), op("and", 1, UNDEF), op("or", 0, UNDEF),
  op("!", []), op("!", [[]]), op("!", {}), op("!", "0"), op("!", "false"), op("!!", []), op("!!", [0]),
  op("!!", NOT_A_NUMBER), op("!!", NEG_ZERO), op("!", UNDEF), op("!!", UNDEF),
  { "": [] }, { "": 1 }, { "a.b": [] }, { ".a": [] }, { "a.": [] }, { a: [], b: [] }, { constructor: [] },
  { hasOwnProperty: [] }, { toString: [] }, raw('{"__proto__":[]}'), { "var.x": [] }, { "var.length": ["a"] },
  { "map.x": [] }, { "+.x": [] }, { "a.constructor": [] }, { "constructor.name": [] },
  { "hasOwnProperty.call": [] }, { "toString.call": [] }, { "": "" }, { " ": 1 }, { "😀": 1 },
  { "==": 1 }, { "==": [] }, { "==": null },
  raw('{"+":[1],"-":[2]}'),
  op("log", 1), op("log", "a"), op("log", [1]), op("log", NEG_ZERO),
];
for (const f of FLOW) add("flow", "mixed", 0, f, { a: 1 });
for (const f of FLOW) add("flow", "mixed", 0, [f, f], { a: 1 });
for (const f of FLOW) add("flow", "mixed", 0, { "cat": [f, "x"] }, { a: 1 });
// literal arrays/objects as operands and results
for (const v of [[], [1, [2]], [{ var: "a" }], [[{ var: "a" }]], { a: { var: "a" } }, { a: 1, b: 2 }, { a: [] },
  [{ "+": [1, 2] }, { "+": [3, 4] }], "str", 5, null, true, [null, UNDEF], [NEG_ZERO], [NOT_A_NUMBER, POS_INF]]) {
  add("literal", "lit", 0, v, { a: 7 });
  add("literal", "lit", 1, op("merge", v), { a: 7 });
  add("literal", "lit", 1, op("cat", v), { a: 7 });
  add("literal", "lit", 1, op("max", v), { a: 7 });
  add("literal", "lit", 1, op("if", true, v, 0), { a: 7 });
}

// 12. Deep nesting (up to the library's documented limit) and wide rules.
for (const depth of [1, 2, 5, 10, 20, 30, 50, 60, 62]) {
  let r = 1;
  for (let i = 0; i < depth; i++) r = op("+", r, 1);
  add("deep", "+", 2, r, null);
  let q = { var: "a" };
  for (let i = 0; i < depth; i++) q = op("if", true, q, 0);
  add("deep", "if", 3, q, { a: 3 });
  let l = [1];
  for (let i = 0; i < depth; i++) l = [l];
  add("deep", "lit", 0, l, null);
  add("deep", "merge", 1, op("merge", l), null);
}
for (const width of [1, 10, 100, 1000, 10000]) {
  add("wide", "+", width, opRaw("+", Array.from({ length: width }, (_, i) => i)), null);
  add("wide", "cat", width, opRaw("cat", Array.from({ length: width }, (_, i) => i % 10)), null);
  add("wide", "max", width, opRaw("max", Array.from({ length: width }, (_, i) => (i * 7919) % 1000)), null);
  add("wide", "and", width, opRaw("and", Array.from({ length: width }, () => 1)), null);
  add("wide", "merge", width, opRaw("merge", Array.from({ length: width }, (_, i) => [i])), null);
  add("wide", "lit", 0, Array.from({ length: width }, (_, i) => i), null);
  add("wide", "map", 2, op("map", Array.from({ length: width }, (_, i) => i), { "+": [{ var: "" }, 1] }), null);
  add("wide", "reduce", 3, op("reduce", Array.from({ length: width }, (_, i) => i),
    { "+": [{ var: "current" }, { var: "accumulator" }] }, 0), null);
}
add("wide", "max", 1000, opRaw("max", Array.from({ length: 1000 }, (_, i) => (i % 2 ? NEG_ZERO : 0))), null);

// 13. Coercion matrix: loose and strict equality, ordering, over typed values.
const TYPED = [
  null, UNDEF, true, false, 0, 1, -1, NEG_ZERO, NOT_A_NUMBER, POS_INF, NEG_INF, "", "0", "1", "-1", "a",
  " ", "true", "false", "null", "NaN", "Infinity", "0x0", [], [0], [1], ["a"], [[]], [null], [[0]], {}, { a: 1 },
];
for (const a of TYPED) for (const b of TYPED) {
  for (const o of ["==", "===", "!=", "!==", "<", "<=", ">", ">=", "+", "-", "*", "/", "%", "in", "cat", "max", "min", "substr"]) {
    add("coercion", o, 2, op(o, a, b), null);
  }
}

// 14. Control flow over an operand the reference throws on only when it
// evaluates it (an empty *), in branches that are and are not taken. Last, so
// that adding it left the ids of every earlier case unchanged.
const LAZY_THROW = [
  op("if", true, 1, EMPTY_MUL), op("if", false, EMPTY_MUL, 2), op("if", EMPTY_MUL, 1, 2),
  op("or", 1, EMPTY_MUL), op("or", 0, EMPTY_MUL), op("and", 0, EMPTY_MUL), op("and", 1, EMPTY_MUL),
  op("?:", true, 1, EMPTY_MUL), op("map", [], EMPTY_MUL), op("map", [1], EMPTY_MUL),
  op("filter", [], EMPTY_MUL), op("all", [], EMPTY_MUL), op("some", [], EMPTY_MUL), op("none", [], EMPTY_MUL),
  op("reduce", [], EMPTY_MUL, 0), op("reduce", [1], EMPTY_MUL, 0), op("+", 1, EMPTY_MUL), op("==", EMPTY_MUL, 1),
];
for (const f of LAZY_THROW) add("flow", "mixed", 0, f, { a: 1 });
for (const f of LAZY_THROW) add("flow", "mixed", 0, [f, f], { a: 1 });
for (const f of LAZY_THROW) add("flow", "mixed", 0, { "cat": [f, "x"] }, { a: 1 });

// 15. Every rule of the community test suite (test/community, from
// json-logic/compat-tables), with the reference's own answers. Most of the
// suite is written for the JSON Logic 2.0 dialect; what matters here is only
// that lenient mode answers each rule as json-logic-js does.
{
  const path = require("path");
  const dir = path.join(__dirname, "..", "test", "community");
  for (const file of JSON.parse(fs.readFileSync(path.join(dir, "index.json"), "utf8"))) {
    for (const entry of JSON.parse(fs.readFileSync(path.join(dir, file), "utf8"))) {
      if (entry === null || typeof entry !== "object") continue;
      const rule = entry.rule;
      const name = rule !== null && typeof rule === "object" && !Array.isArray(rule) && Object.keys(rule).length === 1
        ? Object.keys(rule)[0] : "lit";
      add("community", name, 0, rule, entry.data === undefined ? null : entry.data);
    }
  }
}

// ---------------------------------------------------------------------
// Inherited-property deviation (docs/COMPATIBILITY.md, DEV-INHERITED).
//
// In JavaScript, {"var":"constructor"} on {} finds Object's constructor, and
// {"var":"__proto__"} finds Object.prototype, because property lookup walks
// the prototype chain. JSON data has no prototype chain in any other
// language, so jsonlogic-hs looks at own properties only. For every case
// whose variable paths use such a name (and whose data does not itself have
// a key of that name) the fixture records, besides the reference's answer,
// `expect_own`: the reference's answer when the name is replaced by a key
// that is certainly absent. The Haskell runner holds the library to
// `expect_own` for those cases.
// ---------------------------------------------------------------------
const INHERITED_NAMES = new Set([
  "constructor", "__proto__", "toString", "valueOf", "hasOwnProperty", "toLocaleString", "isPrototypeOf",
  "propertyIsEnumerable", "__defineGetter__", "__defineSetter__", "__lookupGetter__", "__lookupSetter__",
  "map", "concat", "indexOf", "charAt", "trim", "filter", "slice", "join",
]);
const ABSENT = "\u0001absent:";

let currentDataText = "";
function renamePath(path, used) {
  if (typeof path !== "string") return path;
  return path.split(".").map((seg) => {
    if (!INHERITED_NAMES.has(seg) || currentDataText.includes(JSON.stringify(seg))) return seg;
    used.add(seg);
    return ABSENT + seg;
  }).join(".");
}
function renameRule(rule, used) {
  if (Array.isArray(rule)) return rule.map((r) => renameRule(r, used));
  if (rule === null || typeof rule !== "object") return rule;
  const keys = Object.keys(rule);
  if (keys.length !== 1) return rule;
  const name = keys[0];
  let args = rule[name];
  const wasArray = Array.isArray(args);
  if (!wasArray) args = [args];
  let out;
  if (name === "var") {
    out = args.map((a, i) => (i === 0 ? renamePath(renameRule(a, used), used) : renameRule(a, used)));
  } else if (name === "missing") {
    out = args.map((a) => (Array.isArray(a) ? a.map((k) => renamePath(renameRule(k, used), used)) : renamePath(renameRule(a, used), used)));
  } else if (name === "missing_some") {
    out = args.map((a, i) => (i === 1 && Array.isArray(a) ? a.map((k) => renamePath(renameRule(k, used), used)) : renameRule(a, used)));
  } else {
    out = args.map((a) => renameRule(a, used));
  }
  return { [name]: wasArray ? out : out[0] };
}

// ---------------------------------------------------------------------
// Run the oracle and write the fixture.
// ---------------------------------------------------------------------
function wf(x) {
  if (typeof x === "string") return typeof x.toWellFormed === "function" ? x.toWellFormed() : x;
  if (Array.isArray(x)) return x.map(wf);
  if (x !== null && typeof x === "object") {
    const o = {};
    for (const k of Object.keys(x)) {
      // defineProperty, not assignment: a key named __proto__ must stay an own property
      Object.defineProperty(o, wf(k), { value: wf(x[k]), enumerable: true, writable: true, configurable: true });
    }
    return o;
  }
  return x;
}
function canon(x) {
  // JSON text of a reference result; undefined (and functions) are null.
  const t = JSON.stringify(wf(x === undefined ? null : x));
  return t === undefined ? "null" : t;
}
function run(rule, data) {
  try {
    return '{"ok":' + canon(jsonLogic.apply(rule, data)) + "}";
  } catch (error) {
    const message = String(error && error.message ? error.message : error);
    return '{"error":' + JSON.stringify(wf(message)) + "}";
  }
}

// jsonLogic.apply calls console.log for the "log" operator; keep stdout clean.
console.log = () => {};

function fnv(text) {
  let h = 0x811c9dc5;
  for (let i = 0; i < text.length; i++) {
    h ^= text.charCodeAt(i);
    h = Math.imul(h, 0x01000193) >>> 0;
  }
  return h >>> 0;
}

// Tiers. `core` is committed and replayed by `cabal test`; `full` is the
// whole catalogue, replayed by the nightly job. Small, hand-picked families
// are always kept whole; the big cross-product families are thinned by a
// stable hash of the case text, so the sample never changes unless the
// catalogue does.
const THIN = { pair: 8, triple: 6, unicode: 6, coercion: 8, var: 6, collection: 8, signedzero: 3, numstr: 2, unary: 2 };
const tier = (process.env.OUTLIER_TIER || "core");
function kept(c) {
  if (tier === "full") return true;
  const k = THIN[c.fam];
  return !k || fnv(c.ruleText + "\u0001" + c.dataText) % k === 0;
}

const proto = (c, rule) => {
  if (!/"(?:[^"\\]*\.)?(?:constructor|__proto__|toString|valueOf|hasOwnProperty|toLocaleString|isPrototypeOf|propertyIsEnumerable|map|concat|indexOf|charAt|trim|filter|slice|join)(?:\.[^"\\]*)?"/.test(c.ruleText)) return null;
  currentDataText = c.dataText;
  const used = new Set();
  const renamed = renameRule(rule, used);
  if (used.size === 0) return null;
  let text = run(renamed, JSON.parse(c.dataText));
  for (const name of used) text = text.split(JSON.stringify(ABSENT + name).slice(1, -1)).join(name);
  return text;
};

// DEV-REEVAL: the reference implements missing / missing_some by calling
// apply({"var": key}) on keys that were already evaluated, so a key that is
// itself a one-key object (data!) is run as a rule. jsonlogic-hs never runs
// data as rules. Cases where that happens are tagged and not compared.
function hasLogicDeep(x) {
  if (jsonLogic.is_logic(x)) return true;
  return Array.isArray(x) && x.some(hasLogicDeep);
}
function reevaluates(rule, data) {
  if (rule === null || typeof rule !== "object" || Array.isArray(rule)) return false;
  const keys = Object.keys(rule);
  if (keys.length !== 1) return false;
  const name = keys[0];
  if (name !== "missing" && name !== "missing_some") return false;
  let args = Array.isArray(rule[name]) ? rule[name] : [rule[name]];
  try {
    const values = args.map((a) => jsonLogic.apply(a, data));
    if (name === "missing") return values.some(hasLogicDeep);
    return values.length > 1 && hasLogicDeep(values[1]);
  } catch (e) {
    return false;
  }
}

const lines = [];
for (const c of cases) {
  if (!kept(c)) continue;
  const rule = JSON.parse(c.ruleText);
  const data = JSON.parse(c.dataText);
  const expect = run(rule, data);
  const own = proto(c, rule);
  let extra = "";
  if (reevaluates(rule, data)) extra = ',"dev":"reeval"';
  else if (own !== null && own !== expect) extra = ',"dev":"inherited","expect_own":' + own;
  lines.push(
    '{"id":' + JSON.stringify(c.id) + ',"fam":' + JSON.stringify(c.fam) + ',"op":' + JSON.stringify(c.op) +
    ',"n":' + c.n + ',"rule":' + c.ruleText + ',"data":' + c.dataText + ',"expect":' + expect + extra + "}"
  );
}
const meta = '{"meta":{"cases":' + lines.length + ',"oracle":' + JSON.stringify(REQUIRED_ORACLE) + ',"tier":' + JSON.stringify(tier) + "}}";
const output = [meta, ...lines].join("\n") + "\n";

const args = process.argv.slice(2);
if (args[0] === "--cases") {
  // Raw rule/data pairs, one per line, for feeding other implementations.
  fs.writeFileSync(args[1], cases.filter(kept).map((c) => '{"rule":' + c.ruleText + ',"data":' + c.dataText + "}").join("\n") + "\n");
  process.stderr.write("wrote " + lines.length + " raw cases to " + args[1] + "\n");
} else if (args[0] === "--check") {
  const current = fs.readFileSync(args[1], "utf8");
  if (current !== output) {
    process.stderr.write("outlier fixture " + args[1] + " is stale: regenerate with `node outliers.js " + args[1] + "`\n");
    process.exit(1);
  }
  process.stderr.write("outlier fixture is up to date (" + lines.length + " cases, tier " + tier + ")\n");
} else if (args.length === 1) {
  fs.writeFileSync(args[0], output);
  process.stderr.write("wrote " + lines.length + " cases to " + args[0] + " (" + output.length + " bytes, tier " + tier + ")\n");
} else {
  process.stderr.write("usage: [OUTLIER_TIER=core|full] node outliers.js OUTPUT.jsonl | --check FIXTURE | --cases FILE\n");
  process.exit(2);
}
