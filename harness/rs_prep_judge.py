"""Two modes.
  prep:  write cases_all.jsonl for the Rust runner (raw rule/data text, so values like 1e309 survive)
  judge: score a Rust result file against the fixtures.
Usage: python rs_prep_judge.py prep
       python rs_prep_judge.py judge LABEL RESULTS.jsonl OUT.json
"""
import json, math, sys, collections

ROOT = "/home/claude/jsonlogic-hs/test"
dec = json.JSONDecoder()


def raw_field(line, key):
    """Return the raw text of the value of top-level `"key":` in an outlier line."""
    i = line.index(f'"{key}":') + len(key) + 3
    _, end = dec.raw_decode(line, i)
    return line[i:end]


def load_cases():
    cases = []  # (id, kind, raw_rule, raw_data, meta)
    n = 0
    for t in json.load(open(f"{ROOT}/tests.json")):
        if isinstance(t, list):
            cases.append((f"classic:{n}", "classic", json.dumps(t[0]), json.dumps(t[1]), {"expect": t[2]}))
            n += 1
    lines = [l for l in open(f"{ROOT}/outliers.jsonl", encoding="utf-8").read().split("\n") if l.strip()]
    for l in lines[1:]:
        c = json.loads(l)
        cases.append((f"out:{c['id']}", "outlier", raw_field(l, "rule"), raw_field(l, "data"), c))
    n = 0
    for fn in json.load(open(f"{ROOT}/community/index.json")):
        for c in json.load(open(f"{ROOT}/community/{fn}")):
            if isinstance(c, str):
                continue
            cases.append((f"comm:{n}", "community", json.dumps(c["rule"]), json.dumps(c.get("data")), c))
            n += 1
    return cases


def norm(x):
    if x is None: return None
    if isinstance(x, bool): return x
    if isinstance(x, int):
        try: return float(x)
        except OverflowError: return None
    if isinstance(x, float):
        if math.isnan(x) or math.isinf(x): return None
        return 0.0 if x == 0 else x
    if isinstance(x, str): return x.encode("utf-8", "replace").decode("utf-8")
    if isinstance(x, list): return [norm(v) for v in x]
    if isinstance(x, dict): return {k: norm(v) for k, v in x.items()}
    return x


def deep_eq(a, b):
    if isinstance(a, bool) or isinstance(b, bool):
        return isinstance(a, bool) and isinstance(b, bool) and a == b
    if isinstance(a, float) and isinstance(b, float): return a == b
    if isinstance(a, list) and isinstance(b, list):
        return len(a) == len(b) and all(deep_eq(x, y) for x, y in zip(a, b))
    if isinstance(a, dict) and isinstance(b, dict):
        return a.keys() == b.keys() and all(deep_eq(a[k], b[k]) for k in a)
    return a == b and type(a) == type(b)


# compat-tables (JS runner) value comparison
def cmp_values(got, expected):
    if expected is None:
        return got is None or got is False or got == 0 and not isinstance(got, bool) or got == "" or (isinstance(got, list) and len(got) == 0)
    if isinstance(got, (int, float)) and isinstance(expected, (int, float)) and not isinstance(got, bool) and not isinstance(expected, bool):
        return abs(got - expected) < 1e-10
    if isinstance(got, list) and isinstance(expected, list):
        return len(got) == len(expected) and all(cmp_values(g, e) for g, e in zip(got, expected))
    if isinstance(got, dict) and isinstance(expected, dict):
        return got.keys() == expected.keys() and all(cmp_values(got[k], expected[k]) for k in got)
    return got == expected and type(got) == type(expected)


def main():
    if sys.argv[1] == "prep":
        with open("/home/claude/bench/cases_all.jsonl", "w", encoding="utf-8") as f:
            for cid, kind, r, d, _ in load_cases():
                f.write(json.dumps({"id": cid, "rule": r, "data": d}, ensure_ascii=False) + "\n")
        print("cases written")
        return
    label, resfile, outpath = sys.argv[2], sys.argv[3], sys.argv[4]
    results = {}
    for l in open(resfile, encoding="utf-8"):
        o = json.loads(l)
        results[o["id"]] = o
    res = {"classic": {"pass": 0, "total": 0},
           "outliers": {"pass": 0, "total": 0, "fam": {}, "fails": [], "unparsable": 0, "panics": 0},
           "community": {"pass": 0, "total": 0}}
    famfails = collections.Counter()
    for cid, kind, r, d, c in load_cases():
        o = results[cid]
        st = o["status"]
        got = None
        if st == "ok":
            try: got = json.loads(o["value"])
            except Exception: st = "error"
        if kind == "classic":
            res["classic"]["total"] += 1
            res["classic"]["pass"] += st == "ok" and deep_eq(norm(got), norm(c["expect"]))
        elif kind == "outlier":
            f = res["outliers"]["fam"].setdefault(c["fam"], {"pass": 0, "total": 0})
            f["total"] += 1; res["outliers"]["total"] += 1
            res["outliers"]["unparsable"] += st == "unparsable"
            res["outliers"]["panics"] += st == "panic"
            exp = c["expect"]
            if "error" in exp: ok = st != "ok"
            elif st != "ok": ok = False
            else:
                ok = deep_eq(norm(got), norm(exp["ok"])) or (c.get("dev") == "inherited" and "expect_own" in c and deep_eq(norm(got), norm(c["expect_own"]["ok"])))
            if ok:
                f["pass"] += 1; res["outliers"]["pass"] += 1
            else:
                famfails[c["fam"]] += 1
                if famfails[c["fam"]] <= 3:
                    res["outliers"]["fails"].append({"id": c["id"], "fam": c["fam"], "rule": c["rule"], "data": c["data"], "expect": exp,
                                                      "got": {"status": st, "value": got if st == "ok" else o["value"][:80]}})
        else:
            res["community"]["total"] += 1
            has_err = c.get("error") is not None
            if has_err:
                if st == "ok": ok = False
                else:
                    t = (c["error"].get("type") if isinstance(c["error"], dict) else None) or ""
                    ok = st in ("error", "panic", "unparsable") and (st != "error" or not t or t.lower() in o["value"].lower())
            else:
                ok = st == "ok" and cmp_values(got, c.get("result"))
            res["community"]["pass"] += ok
    json.dump({label: res}, open(outpath, "w"), indent=1)
    print(label, "classic", f'{res["classic"]["pass"]}/{res["classic"]["total"]}',
          "outliers", f'{res["outliers"]["pass"]}/{res["outliers"]["total"]}',
          f'(unparsable {res["outliers"]["unparsable"]}, panics {res["outliers"]["panics"]})',
          "community", f'{res["community"]["pass"]}/{res["community"]["total"]}')


main()
