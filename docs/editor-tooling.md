# Editor tooling

lg includes a standard Language Server Protocol endpoint over stdin/stdout:

```sh
dune exec lg -- --lsp
```

An installed or copied executable can be started directly with
`lg --lsp`.

The server currently advertises full text-document synchronization and
publishes compiler-backed diagnostics for:

- `textDocument/didOpen`
- `textDocument/didChange`
- `textDocument/didSave`
- `textDocument/didClose`

Diagnostics use the same reader, elaboration, package discovery, Parsetree, and
OCaml compiler-libs typecheck path as the CLI. Both errors and enabled OCaml
warnings are published; warnings include host-owned checks such as
non-exhaustive and redundant pattern matches. Lines and columns follow the LSP
zero-based convention. Closing a document clears its diagnostics.

The same cached OCaml Typedtree analysis powers:

- `textDocument/hover`, with OCaml-inferred types;
- `textDocument/definition`, including compiler-resolved value definitions;
- `textDocument/completion`, with lg source labels and OCaml type details;
- `textDocument/signatureHelp`, with OCaml-inferred parameter and return types;
- `textDocument/references` and `textDocument/documentHighlight`, using OCaml
  symbol identities so shadowed bindings remain distinct;
- `textDocument/prepareRename` and `textDocument/rename`, with exact source
  symbol edits;
- `textDocument/documentSymbol` and `workspace/symbol`, preserving lg names;
- `textDocument/semanticTokens/full`, with compiler-resolved namespaces, types,
  functions, parameters, fields, constructors, protocols, and methods;
- `textDocument/formatting`, with deterministic 80-column formatting that
  preserves comments and string contents.
- `textDocument/codeAction`, with preferred quick fixes for missing closing
  list, vector, and map delimiters.

Positions are converted between UTF-8 source offsets and the UTF-16 code units
required by LSP. A document is parsed, elaborated, and typechecked once per full
content update; all semantic queries reuse that analysis.

While the final top-level form is incomplete, diagnostics report the precise
opening delimiter and expected closer. Semantic requests continue to use the
successfully parsed and typechecked top-level prefix, so hover, completion, and
navigation remain available for definitions above the edit.

When the client supports dynamic watched-file registration, the server
registers `**/*.cljc` after initialization. File creation, changes, deletion,
and renames then update the dependency index and republish affected diagnostics.

## Neovim

Assign a `lg` filetype and start the server from the project root:

```lua
vim.filetype.add({ extension = { lg = "lg" } })

vim.api.nvim_create_autocmd("FileType", {
  pattern = "lg",
  callback = function()
    vim.lsp.start({
      name = "lg",
      cmd = { "lg", "--lsp" },
      root_dir = vim.fs.root(0, { "dune-project", ".git" }),
    })
  end,
})
```

For development inside this repository, replace `cmd` with:

```lua
cmd = { "dune", "exec", "lg", "--", "--lsp" }
```

## Emacs Eglot

lg ships a small self-contained Emacs major mode at
[`editors/emacs/lg-mode.el`](../editors/emacs/lg-mode.el). It provides Lisp
editing defaults for `.cljc` and `.lgi` files and registers `lg-mode` with
Eglot.

```elisp
(add-to-list 'load-path "/path/to/lg/editors/emacs")
(require 'lg-mode)
```

`lg-mode.el` opts out of Emacs native compilation with `no-native-compile: t`
and a JIT deny-list entry. This avoids distracting gcc/libgccjit warnings on
machines where Emacs native compilation is installed but the compiler driver is
not usable.

The `.cljc` and `.lgi` auto-mode entries are project-aware. Files under an lg
compiler checkout, a project with a `.lg-project` marker, a Dune project whose
`dune-project` depends on `lg`, or an opam package whose `depends` field
contains `"lg"` open in `lg-mode`. Other `.cljc` files are dispatched to
`lg-cljc-fallback-mode`, which defaults to `clojure-mode` and falls back to
`lisp-mode` when `clojure-mode` is not installed.

Inside the lg compiler checkout, `lg-eglot-setup` starts the language server
with `dune exec lg -- --lsp` so Emacs uses the current worktree. Outside the
checkout, it falls back to the installed command:

```elisp
(setq lg-lsp-command '("lg" "--lsp"))
```

When a project-local saved compiler state is present, `lg-mode` appends
`--state <path>` to the Eglot command. This lets downstream Dune/opam projects
reuse their compiled lg dependency environment. The default search list includes
Logseq Chat's installed `signal-lg` state before falling back to installed lg
stdlib state, and can be customized with `lg-project-state-relative-paths`.

Run `M-x eglot` in an `lg-mode` buffer, or add this hook if you want Eglot to
start automatically whenever a `.cljc` file opens:

```elisp
(add-hook 'lg-mode-hook #'eglot-ensure)
```

Once Eglot is connected, standard Emacs commands use the lg language server:

- `M-.` (`xref-find-definitions`) jumps to definitions.
- `M-?` (`xref-find-references`) finds references.
- `M-x eglot-rename` renames the symbol at point.
- `M-x eglot-format-buffer` formats the current buffer.
- `M-x flymake-show-buffer-diagnostics` shows compiler and OCaml diagnostics.

`lg-mode` also adds direct lg commands:

- `C-c C-t` (`lg-show-type-at-point`) shows the LSP hover type at point.
- `C-c C-b` (`lg-eval-buffer`) evaluates the current buffer.
- `C-c C-r` (`lg-eval-region`) evaluates the selected region.
- `C-c C-z` (`lg-repl`) starts or switches to an lg terminal REPL.
- `C-c C-n` (`lg-connect-nrepl`) starts an lg nREPL server and connects with
  the optional `neat` package.

Inside this repository, eval commands build the Dune-generated stdlib artifacts
when needed and run the current checkout with `--run-from`; REPL commands use
the same checked-out stdlib state. Outside the repository, eval appends a
temporary `.cljc` file to `lg-eval-command`, which defaults to `lg --run`, and
REPL commands use `lg-repl-command` or `lg-nrepl-command`.

The core Reason editor baseline—types, formatting, diagnostics, completion, and
jump-to-definition—is present. The server indexes `.cljc` and `.lgi` files below
the workspace root, skips build/vendor directories, orders explicit module and
namespace dependencies from compiler state, isolates invalid files, and provides
cross-file definitions, references, rename, and workspace symbols, semantic
tokens, signature help, delimiter quick fixes, and recoverable semantic analysis
for completed top-level forms.
