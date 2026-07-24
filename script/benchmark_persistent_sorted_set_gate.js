#!/usr/bin/env node
import fs from "node:fs";

const runtimeNames = ["lg-native", "lg-melange"];

function parseResults(output) {
  const results = {};
  let runtime = null;

  for (const rawLine of output.split(/\r?\n/)) {
    const line = rawLine.trim();
    const runtimeMatch = /^runtime\s+([^\s]+)$/.exec(line);
    if (runtimeMatch) {
      runtime = runtimeMatch[1];
      results[runtime] = results[runtime] || {};
      continue;
    }

    if (runtime === null) continue;
    const resultMatch = /^(.+?)(?::|\s+)\s*([^\s]+)$/.exec(line);
    if (resultMatch) {
      results[runtime][resultMatch[1]] = Number(resultMatch[2]);
    }
  }

  return results;
}

function check(output) {
  const results = parseResults(output);
  const upstream = results["upstream-cljs"];
  if (!upstream) return ["missing upstream-cljs benchmark results"];

  const failures = [];
  const benchmarkNames = Object.keys(upstream);
  for (const name of benchmarkNames) {
    if (!Number.isFinite(upstream[name])) {
      failures.push(`invalid upstream-cljs ${name} result`);
    }
  }

  for (const runtimeName of runtimeNames) {
    const runtime = results[runtimeName];
    if (!runtime) {
      failures.push(`missing ${runtimeName} benchmark results`);
      continue;
    }

    for (const name of benchmarkNames) {
      const actual = runtime[name];
      const target = upstream[name];
      if (!Number.isFinite(actual)) {
        failures.push(`missing ${runtimeName} ${name}`);
      } else if (Number.isFinite(target) && !(actual < target)) {
        failures.push(
          `${runtimeName} ${name} ${actual}ms is not faster than upstream-cljs ${target}ms`,
        );
      }
    }
  }

  return failures;
}

const failures = check(fs.readFileSync(0, "utf8"));
if (failures.length > 0) {
  console.error(failures.join("\n"));
  process.exit(1);
}

export { check, parseResults };
