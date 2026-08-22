let () =
  Lgc_target_tests.run ();
  Compiler_concurrency_tests.run ();
  Compiler_fuzz_tests.run ()
