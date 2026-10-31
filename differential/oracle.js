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
// a severed half of a surrogate pair is U+FFFD, as String.prototype.toWellFormed
// makes it. This is the one documented string deviation, and it has to hold
// at the point the half is produced, not only in the final answer: a half
// that is then compared, measured or searched behaves as U+FFFD, not as the
// lone surrogate (as a string, "\uD83D" sorts below "\uD83D\uDE00", but U+FFFD
// sorts above it). Strings are cut in two places, indexing by `var` and
// `substr`, so those two operations are replaced by the reference's own code
// with the cut made well-formed; everything else is the reference untouched.
function sever(x) {
  return typeof x === "string" && typeof x.toWellFormed === "function" ? x.toWellFormed() : x;
}

// json-logic-js 2.0.5, "var", with the result well-formed.
jsonLogic.add_operation("var", function (a, b) {
  "use strict"; // as the reference is: `this` stays a primitive, not a boxed one
  const notFound = b === undefined ? null : b;
  let data = this;
  if (typeof a === "undefined" || a === "" || a === null) {
    return sever(data);
  }
  const subProps = String(a).split(".");
  for (let i = 0; i < subProps.length; i++) {
    if (data === null || data === undefined) {
      return notFound;
    }
    data = data[subProps[i]];
    if (data === undefined) {
      return notFound;
    }
  }
  return sever(data);
});

// json-logic-js 2.0.5, "substr", with the result well-formed.
jsonLogic.add_operation("substr", function (source, start, end) {
  "use strict";
  if (end < 0) {
    const temp = String(source).substr(start);
    return sever(temp.substr(0, temp.length + end));
  }
  return sever(String(source).substr(start, end));
});

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
