
# Dolmen LSP server

The dolmen lsp server is part of the `dolmen_lsp` opam package.

## Installation

The Dolmen LSP is currently experiemental, so you'll need to install its dev version.

You need [opam](https://opam.ocaml.org/) to install the developpement version of dolmen.
You can installations instructions for opam [here](https://opam.ocaml.org/doc/Install.html).

Once you have opam, you can install `dolmen_lsp` with the following command:

    opam pin add https://github.com/Gbury/dolmen.git

This will install (among other things) the dolmen lsp server in binary called
`dolmenls` (for dolmen language server).

## Demo

<a href="https://asciinema.org/a/MoaXpOZJnrJDyrB5gZneuWZxd" target="_blank"><img src="https://asciinema.org/a/MoaXpOZJnrJDyrB5gZneuWZxd.svg" /></a>

## Configuration

### Vim & Neovim

### 'autozimu/LanguageClient-neovim'

```vim
let g:LanguageClient_serverCommands = {
\ 'smt2': ['dolmenls'],
\ 'p': ['dolmenls'],
\ 'cnf': ['dolmenls'],
\ 'zf': ['dolmenls'],
\ }
```

#### ALE setup

```vim
for ft in ['smt2', 'tptp', 'p', 'cnf', 'icnf', 'zf']
  call ale#linter#Define(ft, {
  \   'name': 'dolmenls',
  \   'lsp': 'stdio',
  \   'executable': '/path/to/dolmenls',
  \   'command': '%e',
  \   'project_root': '.',
  \})
endfor
```

For a remote connection (replace the `'address'` field):

```vim
for ft in ['smt2', 'tptp', 'p', 'cnf', 'icnf', 'zf']
  call ale#linter#Define(ft, {
  \   'name': 'dolmenls',
  \   'lsp': 'socket',
  \   'address': '127.0.0.1:8854',
  \   'project_root': '.',
  \})
endfor
```

#### vim-lsp setup

```vim
if executable('dolmenls')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'dolmenls',
        \ 'cmd': {server_info->['dolmenls']},
        \ 'whitelist': ['smt2','tptp', 'dimacs', 'icnf', 'zf'],
        \ })
endif
```

### nvim-lspconfig
Add `dolmenls` to your list of language servers. For instance, you can add

```lua
require'lspconfig'.dolmenls.setup{}
```

to your `init.lua`.

#### Generic configuration for supported languages

Since some of the languages supported by dolmen are not very mainstream,
you might need the following configuration to correctly auto-detect languages:

```vim
au BufEnter *.cnf setf dimacs
au BufEnter *.icnf setf icnf
au BufEnter *.p setf tptp
au BufEnter *.smt2 setf smt2
au BufEnter *.zf setf zf
```

If you prefer lua to vimscript, you can proceed as follow.
First, turn on the filetype configuration feature in your `init.lua`:

```lua
vim.g.do_filetype_lua = 1
```
Then add the filetypes in the file `$HOME/.config/nvim/lua/filetype.lua`

```lua
vim.filetype.add({
  extension = {
    cnf = "dimacs",
    icnf = "icnf",
    p = "tptp",
    smt2 = "smt2",
    zf = "zf",
    qmd = "markdown"
  },
})
```

### Emacs

Since emacs has no built-in LSP support, you will need to install
the [lsp-mode](https://github.com/emacs-lsp/lsp-mode) first. These instructions also
assume that [smtlib-mode](https://github.com/mebsout/smtlib-mode) is installed. Then extend your
`.emacs` configuration with the following lines after `lsp-mode` and `smtlib-mode` have been initialized:

```
;; connect dolmen as lsp backend for smtlib-mode
(add-hook 'smtlib-mode-hook #'lsp)

(add-to-list 'lsp-language-id-configuration '(smtlib-mode . "smtlib"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "dolmenls")
                  :major-modes '(smtlib-mode)
                  :priority 0
                  :server-id 'dolmenls
                  ))
```

### VS Code

Install the VS Code extension SMT-LSP, through the marketplace or by launching VS Code Quick Open (Ctrl+P) and running: `ext install hra687261.smt-lsp`

The extension is available:
- On the VS Code marketplace: https://marketplace.visualstudio.com/items?itemName=hra687261.smt-lsp
- On the Open VSX Registry: https://open-vsx.org/extension/hra687261/smt-lsp.
