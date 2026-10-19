#!/usr/bin/env node
// The reference implementation as an oracle.
//
// Usage: node oracle.js CASES RESULTS
//
// CASES has one JSON object per line: {"rule": ..., "data": ...}.
// RESULTS gets one line per case, in order: {"ok": value} or
// {"error": message}. JavaScript `undefined` is written as null, and NaN
// and the infinities become null through JSON.stringify, which is exactly
// how jsonlogic-hs renders them.

const fs = require("fs");
const jsonLogic = require("json-logic-js");
console.log = () => {}; // the "log" operator would otherwise write to stdout

const [casesFile, resultsFile] = process.argv.slice(2);
if (!casesFile || !resultsFile) {
  console.error("usage: node oracle.js CASES RESULTS");
  process.exit(2);
}

// The reference is pinned. Anything else is not the oracle this library is
// held to, so fail instead of comparing against it.
const REQUIRED = "2.0.5";
const version = require("json-logic-js/package.json").version;
if (version !== REQUIRED) {
  console.error("oracle: json-logic-js " + version + " is installed, but " + REQUIRED + " is required (run `npm ci` in differential/)");
  process.exit(3);
}
console.error("oracle: json-logic-js " + version + ", node " + process.version);

// Lone surrogates cannot be held in a Haskell Text (nor decoded by aeson), so
// the reference's answer is normalised the way String.prototype.toWellFormed
// would: each becomes U+FFFD. This is the one documented string deviation.
function wellFormed(x) {
  if (typeof x === "string") return typeof x.toWellFormed === "function" ? x.toWellFormed() : x;
  if (Array.isArray(x)) return x.map(wellFormed);
  if (x !== null && typeof x === "object") {
    const o = {};
    for (const k of Object.keys(x)) {
      Object.defineProperty(o, wellFormed(k), { value: wellFormed(x[k]), enumerable: true, writable: true, configurable: true });
    }
    return o;
  }
  return x;
}

const answers = [];
for (const line of fs.readFileSync(casesFile, "utf8").split("\n")) {
  if (line.trim() === "") continue;
  const { rule, data } = JSON.parse(line);
  let answer;
  try {
    const result = jsonLogic.apply(rule, data);
    answer = { ok: wellFormed(result === undefined ? null : result) };
  } catch (error) {
    answer = { error: String(error && error.message ? error.message : error) };
  }
  answers.push(JSON.stringify(answer));
}
fs.writeFileSync(resultsFile, answers.join("\n") + "\n");
