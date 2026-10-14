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

const [casesFile, resultsFile] = process.argv.slice(2);
if (!casesFile || !resultsFile) {
  console.error("usage: node oracle.js CASES RESULTS");
  process.exit(2);
}

let version = "unknown";
try {
  version = require("json-logic-js/package.json").version;
} catch (ignored) {
  // package.json may not be exported; the version is only for the log line
}
console.error("oracle: json-logic-js " + version + ", node " + process.version);

const answers = [];
for (const line of fs.readFileSync(casesFile, "utf8").split("\n")) {
  if (line.trim() === "") continue;
  const { rule, data } = JSON.parse(line);
  let answer;
  try {
    const result = jsonLogic.apply(rule, data);
    answer = { ok: result === undefined ? null : result };
  } catch (error) {
    answer = { error: String(error && error.message ? error.message : error) };
  }
  answers.push(JSON.stringify(answer));
}
fs.writeFileSync(resultsFile, answers.join("\n") + "\n");
