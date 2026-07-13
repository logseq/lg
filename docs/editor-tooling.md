# Editor tooling

cljml includes a standard Language Server Protocol endpoint over stdin/stdout:

```sh
dune exec bin/cljml_cli.exe -- --lsp
```

An installed or copied executable can be started directly with
`cljml_cli --lsp`.

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
- `textDocument/completion`, with cljml source labels and OCaml type details;
- `textDocument/signatureHelp`, with OCaml-inferred parameter and return types;
- `textDocument/references` and `textDocument/documentHighlight`, using OCaml
  symbol identities so shadowed bindings remain distinct;
- `textDocument/prepareRename` and `textDocument/rename`, with exact source
  symbol edits;
- `textDocument/documentSymbol` and `workspace/symbol`, preserving cljml names;
- `textDocument/semanticTokens/full`, with compiler-resolved namespaces, types,
  functions, parameters, fields, constructors, protocols, and methods;
- `textDocument/formatting`, with deterministic 80-column formatting that
  preserves comments and string contents.

Positions are converted between UTF-8 source offsets and the UTF-16 code units
required by LSP. A document is parsed, elaborated, and typechecked once per full
content update; all semantic queries reuse that analysis.

When the client supports dynamic watched-file registration, the server
registers `**/*.cljml` after initialization. File creation, changes, deletion,
and renames then update the dependency index and republish affected diagnostics.

## Neovim

Assign a `cljml` filetype and start the server from the project root:

```lua
vim.filetype.add({ extension = { cljml = "cljml" } })

vim.api.nvim_create_autocmd("FileType", {
  pattern = "cljml",
  callback = function()
    vim.lsp.start({
      name = "cljml",
      cmd = { "cljml_cli", "--lsp" },
      root_dir = vim.fs.root(0, { "dune-project", ".git" }),
    })
  end,
})
```

For development inside this repository, replace `cmd` with:

```lua
cmd = { "dune", "exec", "bin/cljml_cli.exe", "--", "--lsp" }
```

## Emacs Eglot

```elisp
(add-to-list 'auto-mode-alist '("\\.cljml\\'" . clojure-mode))
(add-to-list 'eglot-server-programs
             '(clojure-mode . ("cljml_cli" "--lsp")))
```

The core Reason editor baseline—types, formatting, diagnostics, completion, and
jump-to-definition—is present. The server indexes `.cljml` files below the
workspace root, orders explicit module dependencies from compiler state, isolates
invalid files, and provides cross-file definitions, references, rename, and
workspace symbols, semantic tokens, and signature help. Code actions and
recoverable parsing remain future ocaml-lsp-parity capabilities.
