// Runs jsonlogic-hs's fixtures against JS JsonLogic implementations.
// Judging follows jsonlogic-hs docs/CONFORMANCE.md; community scoring copies
// compat-tables js-tests/engines/TestRunner.js.
import fs from "node:fs";
import { createRequire } from "node:module";
import { LogicEngine } from "json-logic-engine";
const require = createRequire(import.meta.url);
const jsonLogicJs = require("json-logic-js");

const ROOT = "/home/claude/jsonlogic-hs/test";
console.log = () => {}; // the "log" operator

const wellFormed = (x) => {
  if (typeof x === "string") return x.toWellFormed();
  if (Array.isArray(x)) return x.map(wellFormed);
  if (x !== null && typeof x === "object") {
    const o = {};
    for (const k of Object.keys(x)) Object.defineProperty(o, wellFormed(k), { value: wellFormed(x[k]), enumerable: true, writable: true, configurable: true });
    return o;
  }
  return x;
};
// JSON.stringify semantics: undefined/NaN/Infinity -> null, -0 -> 0
const norm = (r) => JSON.parse(JSON.stringify(wellFormed(r === undefined ? null : r)) ?? "null");

const deepEq = (a, b) => {
  if (a === b) return true;
  if (typeof a === "number" && typeof b === "number") return a === b || (Number.isNaN(a) && Number.isNaN(b));
  if (Array.isArray(a) && Array.isArray(b)) return a.length === b.length && a.every((v, i) => deepEq(v, b[i]));
  if (a && b && typeof a === "object" && typeof b === "object" && !Array.isArray(a) && !Array.isArray(b)) {
    const ka = Object.keys(a), kb = Object.keys(b);
    return ka.length === kb.length && ka.every((k) => Object.prototype.hasOwnProperty.call(b, k) && deepEq(a[k], b[k]));
  }
  return false;
};

const engines = {
  "json-logic-js 2.0.5 (reference)": () => (rule, data) => jsonLogicJs.apply(rule, data),
  "json-logic-engine 5.0.7 (interpreted)": () => {
    const e = new LogicEngine();
    return (rule, data) => e.run(rule, data);
  },
  "json-logic-engine 5.0.7 (compiled)": () => {
    const e = new LogicEngine();
    return (rule, data) => e.build(rule)(data);
  },
};

// ---- compat-tables scoring (verbatim logic) ----
function compareValues(got, expected) {
  if (expected === null) return got === null || got === false || got === 0 || got === "" || (Array.isArray(got) && got.length === 0);
  if (Number.isNaN(got) && Number.isNaN(expected)) return true;
  if (typeof got === "number" && typeof expected === "number") return Math.abs(got - expected) < 1e-10;
  if (Array.isArray(got) && Array.isArray(expected)) {
    if (got.length !== expected.length) return false;
    return got.every((v, i) => compareValues(v, expected[i]));
  }
  if (typeof got === "object" && typeof expected === "object") {
    if (got === null || expected === null) return got === expected;
    const gk = Object.keys(got), ek = Object.keys(expected);
    if (gk.length !== ek.length) return false;
    return gk.every((k) => ek.includes(k) && compareValues(got[k], expected[k]));
  }
  return got === expected;
}
function compareErrors(got, expected) {
  if (!got || typeof expected !== "object") return false;
  if (expected.type === got || expected === got) return true;
  const gl = got.toLowerCase();
  const et = expected?.type?.toLowerCase() ?? "";
  const em = expected?.message?.toLowerCase() ?? "";
  return (!et || gl.includes(et)) && (!em || gl.includes(em));
}
async function communityPass(run, c) {
  const hasErr = c.error !== undefined && c.error !== null;
  try {
    const result = await run(c.rule, c.data);
    if (hasErr) return false;
    return compareValues(result, c.result);
  } catch (error) {
    if (!hasErr) return false;
    if (Number.isNaN(error)) error = { type: "NaN" };
    else if (error && error.message) error = { type: error.message };
    if (JSON.stringify(error) === JSON.stringify(c.error)) return true;
    return compareErrors(error.type, c.error);
  }
}

const out = {};
for (const [name, mk] of Object.entries(engines)) {
  const run = mk();
  const res = { classic: { pass: 0, total: 0 }, outliers: { pass: 0, total: 0, fam: {}, fails: [] }, community: { pass: 0, total: 0 } };

  // classic
  for (const t of JSON.parse(fs.readFileSync(`${ROOT}/tests.json`, "utf8"))) {
    if (!Array.isArray(t)) continue;
    res.classic.total++;
    try {
      if (deepEq(norm(await run(t[0], t[1])), norm(t[2]))) res.classic.pass++;
    } catch {}
  }

  // outliers
  const lines = fs.readFileSync(`${ROOT}/outliers.jsonl`, "utf8").split("\n").filter((l) => l.trim());
  for (const l of lines.slice(1)) {
    const c = JSON.parse(l);
    const f = (res.outliers.fam[c.fam] ??= { pass: 0, total: 0 });
    f.total++; res.outliers.total++;
    let ok = false, got, threw = false;
    try { got = norm(await run(c.rule, c.data)); } catch (e) { threw = true; got = String(e && e.message || e); }
    if (c.expect.error !== undefined) ok = threw;
    else if (!threw) ok = deepEq(got, c.expect.ok) || (c.dev === "inherited" && "expect_own" in c && deepEq(got, c.expect_own.ok));
    if (ok) { f.pass++; res.outliers.pass++; } else if ((f.fails = (f.fails ?? 0) + 1) <= 3) res.outliers.fails.push({ id: c.id, fam: c.fam, rule: c.rule, data: c.data, expect: c.expect, got: threw ? { threw: got } : { ok: got } });
  }

  // community
  const index = JSON.parse(fs.readFileSync(`${ROOT}/community/index.json`, "utf8"));
  for (const f of index) {
    for (const c of JSON.parse(fs.readFileSync(`${ROOT}/community/${f}`, "utf8"))) {
      if (typeof c === "string") continue;
      res.community.total++;
      if (await communityPass(run, c)) res.community.pass++;
    }
  }
  out[name] = res;
  console.error(name, "classic", res.classic.pass + "/" + res.classic.total, "outliers", res.outliers.pass + "/" + res.outliers.total, "community", res.community.pass + "/" + res.community.total);
}
fs.writeFileSync("/home/claude/bench/results_js.json", JSON.stringify(out, null, 1));
