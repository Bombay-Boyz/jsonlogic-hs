"""Run jsonlogic-hs fixtures against the json_logic module installed in this venv.
Usage: <venv>/bin/python run_py.py LABEL OUT.json
Judging follows jsonlogic-hs docs/CONFORMANCE.md; community scoring follows
compat-tables python-tests/engines/test_runner.py."""
import json, math, sys, collections
from json_logic import jsonLogic

ROOT = "/home/claude/jsonlogic-hs/test"
label, outpath = sys.argv[1], sys.argv[2]


def well_formed(s):
    return s.encode("utf-8", "replace").decode("utf-8")


def norm(x):
    """Reduce a result to JSON.stringify-like form; numbers become floats."""
    if x is None:
        return None
    if isinstance(x, bool):
        return x
    if isinstance(x, int):
        try:
            return float(x)
        except OverflowError:
            return None
    if isinstance(x, float):
        if math.isnan(x) or math.isinf(x):
            return None
        return 0.0 if x == 0 else x
    if isinstance(x, str):
        return well_formed(x)
    if isinstance(x, (list, tuple)):
        return [norm(v) for v in x]
    if isinstance(x, dict):
        return {well_formed(str(k)): norm(v) for k, v in x.items()}
    return ("<other>", type(x).__name__)


def deep_eq(a, b):
    if isinstance(a, bool) or isinstance(b, bool):
        return isinstance(a, bool) and isinstance(b, bool) and a == b
    if isinstance(a, float) and isinstance(b, float):
        return a == b
    if isinstance(a, list) and isinstance(b, list):
        return len(a) == len(b) and all(deep_eq(x, y) for x, y in zip(a, b))
    if isinstance(a, dict) and isinstance(b, dict):
        return a.keys() == b.keys() and all(deep_eq(a[k], b[k]) for k in a)
    return a == b and type(a) == type(b)


res = {"classic": {"pass": 0, "total": 0},
       "outliers": {"pass": 0, "total": 0, "fam": {}, "fails": [], "throw_unexpected": 0},
       "community": {"pass": 0, "total": 0}}

# classic
for t in json.load(open(f"{ROOT}/tests.json")):
    if not isinstance(t, list):
        continue
    res["classic"]["total"] += 1
    try:
        if deep_eq(norm(jsonLogic(t[0], t[1])), norm(t[2])):
            res["classic"]["pass"] += 1
    except BaseException:
        pass

# outliers
lines = [l for l in open(f"{ROOT}/outliers.jsonl", encoding="utf-8").read().split("\n") if l.strip()]
famfails = collections.Counter()
for l in lines[1:]:
    c = json.loads(l)
    f = res["outliers"]["fam"].setdefault(c["fam"], {"pass": 0, "total": 0})
    f["total"] += 1
    res["outliers"]["total"] += 1
    threw, got = False, None
    try:
        got = norm(jsonLogic(c["rule"], c["data"]))
    except BaseException as e:  # includes RecursionError
        threw, got = True, f"{type(e).__name__}: {e}"[:80]
    exp = c["expect"]
    if "error" in exp:
        ok = threw
    elif threw:
        ok = False
    else:
        ok = deep_eq(got, norm(exp["ok"])) or (c.get("dev") == "inherited" and "expect_own" in c and deep_eq(got, norm(c["expect_own"]["ok"])))
    if ok:
        f["pass"] += 1
        res["outliers"]["pass"] += 1
    else:
        famfails[c["fam"]] += 1
        if famfails[c["fam"]] <= 3:
            res["outliers"]["fails"].append({"id": c["id"], "fam": c["fam"], "rule": c["rule"], "data": c["data"],
                                              "expect": exp, "got": {"threw": got} if threw else {"ok": got}})

# community (python-tests scoring)
def cmp_values(got, expected):
    if isinstance(got, float) and isinstance(expected, float):
        return abs(got - expected) < 1e-10
    return got == expected

def cmp_errors(got, expected):
    if isinstance(expected, dict) and "type" in expected:
        return expected["type"].lower() in got.lower()
    return False

for fn in json.load(open(f"{ROOT}/community/index.json")):
    for c in json.load(open(f"{ROOT}/community/{fn}")):
        if isinstance(c, str):
            continue
        res["community"]["total"] += 1
        has_err = c.get("error") is not None
        try:
            r = jsonLogic(c["rule"], c.get("data"))
            ok = (not has_err) and cmp_values(r, c.get("result"))
        except BaseException as e:
            ok = has_err and cmp_errors(str(e), c["error"])
        res["community"]["pass"] += ok

json.dump({label: res}, open(outpath, "w"), indent=1)
print(label, "classic", f'{res["classic"]["pass"]}/{res["classic"]["total"]}',
      "outliers", f'{res["outliers"]["pass"]}/{res["outliers"]["total"]}',
      "community", f'{res["community"]["pass"]}/{res["community"]["total"]}')
