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

The current server focuses on diagnostics with nested-expression ranges. Hover,
definition, completion, formatting, and semantic tokens can be added without
changing the transport or document lifecycle.
