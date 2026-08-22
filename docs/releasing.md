# Releasing LG

LG releases are built from a clean, tagged commit after the same checks used by
CI pass locally.

1. Update the changelog with user-visible compiler, runtime, and compatibility
   changes.
2. Run `npm ci` and `opam install . --deps-only --with-test` in a fresh switch.
3. Run `opam exec -- dune build --action-stderr-on-success=must-be-empty
   @runtest`.
4. Run `opam exec -- dune build
   @test/clojure_suite/clojure-test-suite-smoke` and confirm both Native and
   Melange report zero failures and errors.
5. Run `opam exec -- dune build @install` and `opam lint .`.
6. Create an annotated semantic-version tag and publish the generated opam
   packages from that exact commit.

Do not publish when generated opam metadata differs from `dune-project`, when
the working tree is dirty, or when a compiler artifact format change lacks a
version bump and migration note.
