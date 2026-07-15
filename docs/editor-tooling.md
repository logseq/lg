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

```elisp
(add-to-list 'auto-mode-alist '("\\.cljc\\'" . clojure-mode))
(add-to-list 'eglot-server-programs
             '(clojure-mode . ("lg" "--lsp")))
```

The core Reason editor baseline—types, formatting, diagnostics, completion, and
jump-to-definition—is present. The server indexes `.cljc` files below the
workspace root, orders explicit module dependencies from compiler state, isolates
invalid files, and provides cross-file definitions, references, rename, and
workspace symbols, semantic tokens, signature help, delimiter quick fixes, and
recoverable semantic analysis for completed top-level forms.
