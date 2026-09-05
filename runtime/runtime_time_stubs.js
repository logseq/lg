//Provides: lg_monotonic_time_ms
//Requires: caml_failwith
function lg_monotonic_time_ms() {
  if (typeof performance === "undefined" || typeof performance.now !== "function") {
    caml_failwith("a monotonic performance clock is required");
  }
  return performance.now();
}
