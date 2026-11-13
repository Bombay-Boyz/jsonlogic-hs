// usage: rsbench ENGINE CASES.jsonl OUT.jsonl
// CASES lines: {"id": "...", "rule": "<raw json text>", "data": "<raw json text>"}
use serde_json::Value;
use std::io::{BufRead, Write};
use std::panic::{catch_unwind, AssertUnwindSafe};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let engine = args[1].as_str();
    std::panic::set_hook(Box::new(|_| {})); // silence panic output
    let input = std::io::BufReader::new(std::fs::File::open(&args[2]).unwrap());
    let mut out = std::io::BufWriter::new(std::fs::File::create(&args[3]).unwrap());
    for line in input.lines() {
        let line = line.unwrap();
        if line.trim().is_empty() { continue; }
        let c: Value = serde_json::from_str(&line).unwrap();
        let id = c["id"].as_str().unwrap().to_string();
        let rule = c["rule"].as_str().unwrap().to_string();
        let data = c["data"].as_str().unwrap().to_string();
        let r = catch_unwind(AssertUnwindSafe(|| -> (&'static str, String) {
            match engine {
                "datalogic" => match datalogic_rs::eval_str(rule.as_str(), data.as_str()) {
                    Ok(s) => ("ok", s),
                    Err(e) => ("error", format!("{e}")),
                },
                "jsonlogic-rs" => {
                    let (r, d) = match (serde_json::from_str::<Value>(&rule), serde_json::from_str::<Value>(&data)) {
                        (Ok(r), Ok(d)) => (r, d),
                        _ => return ("unparsable", String::new()),
                    };
                    match jsonlogic_rs::apply(&r, &d) {
                        Ok(v) => ("ok", v.to_string()),
                        Err(e) => ("error", format!("{e}")),
                    }
                }
                "jsonlogic" => {
                    let (r, d) = match (serde_json::from_str::<Value>(&rule), serde_json::from_str::<Value>(&data)) {
                        (Ok(r), Ok(d)) => (r, d),
                        _ => return ("unparsable", String::new()),
                    };
                    match jsonlogic::apply(&r, &d) {
                        Ok(v) => ("ok", v.to_string()),
                        Err(e) => ("error", e),
                    }
                }
                _ => panic!("unknown engine"),
            }
        }));
        let (status, value) = match r { Ok(x) => x, Err(_) => ("panic", String::new()) };
        writeln!(out, "{}", serde_json::json!({"id": id, "status": status, "value": value})).unwrap();
    }
}
