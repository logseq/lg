#include <time.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/mlvalues.h>

CAMLprim value lg_monotonic_time_ms(value unit) {
  struct timespec instant;
  (void)unit;
  if (clock_gettime(CLOCK_MONOTONIC, &instant) != 0) {
    caml_failwith("cannot read the monotonic clock");
  }
  return caml_copy_double((double)instant.tv_sec * 1000.0 +
                          (double)instant.tv_nsec / 1000000.0);
}
