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
- `textDocument/completion`, with cljml source labels and OCaml type details.

Positions are converted between UTF-8 source offsets and the UTF-16 code units
required by LSP. A document is parsed, elaborated, and typechecked once per full
content update; hover, definition, and completion reuse that analysis.

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

The next ReasonML parity item is comment-preserving document formatting.
References, rename, semantic tokens, signature help, and code actions remain
future language-service capabilities.
