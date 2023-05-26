<!-- markdownlint-disable -->
<br />
<div align="center">
  <a href="https://github.com/mrcjkb/haskell-snippets.nvim">
    <img src="./logo.svg" alt="haskell-snippets.nvim">
  </a>
  <p align="center">
    <a href="https://github.com/mrcjkb/haskell-tools.nvim/issues">Report Bug</a>
  </p>
  <p>
    <strong>
      My collection of Haskell snippets for <a href="https://github.com/L3MON4D3/LuaSnip">LuaSnip</a>.
      <br />
      Powered by <a href="https://github.com/nvim-treesitter/nvim-treesitter">tree-sitter</a> and LSP.
    </strong>
  </p>
  <h2>🦥</h>
</div>
<!-- markdownlint-restore -->

[![Neovim][neovim-shield]][neovim-url]
[![Lua][lua-shield]][lua-url]
[![Haskell][haskell-shield]][haskell-url]
[![Nix][nix-shield]][nix-url]

[![GPL2 License][license-shield]][license-url]
[![Issues][issues-shield]][issues-url]
[![Build Status][ci-shield]][ci-url]

## Quick Links

- [Installation](#installation)
- [Quick Setup](#quick-setup)
- [Snippets](#snippets)
- [Recommendations](#recommendations)
- [Contributing](./CONTRIBUTING.md)

## Installation

Use luarocks or your favourite plugin manager.

### Required

- `neovim >= 0.8`
- [`luasnip`](https://github.com/L3MON4D3/LuaSnip)

### Optional

- [`haskell-language-server`](https://haskell-language-server.readthedocs.io/en/latest/installation.html)
  and [`haskell-tools.nvim`](https://github.com/mrcjkb/haskell-tools.nvim)
  (some snippets work better with LSP).
- [`nvim-treesitter`](https://github.com/nvim-treesitter/nvim-treesitter)
  and the parser for haskell.

## Quick setup

Add the following to your LuaSnip setup.

```lua
local ls = require('luasnip')
ls.setup {
  -- Add your LuaSnip config
}
local haskell_snippets = require('haskell-snippets').all
ls.add_snippets('haskell', haskell_snippets, { key = 'haskell' })
```

## Snippets

- [ ] TODO

## Contributing

All contributions are welcome!
See [CONTRIBUTING.md](./CONTRIBUTING.md).

## Recommendations

Here are some other plugins I recommend for Haskell development:

- [mrcjkb/haskell-tools.nvim](https://github.com/mrcjkb/haskell-tools.nvim):
  Toolset to improve the Haskell experience in Neovim.
- [luc-tielen/telescope_hoogle](https://github.com/luc-tielen/telescope_hoogle):
  Hoogle search.

<!-- MARKDOWN LNIKS & IMAGES -->
[neovim-shield]: https://img.shields.io/badge/NeoVim-%2357A143.svg?&style=for-the-badge&logo=neovim&logoColor=white
[neovim-url]: https://neovim.io/
[lua-shield]: https://img.shields.io/badge/lua-%232C2D72.svg?style=for-the-badge&logo=lua&logoColor=white
[lua-url]: https://www.lua.org/
[nix-shield]: https://img.shields.io/badge/nix-0175C2?style=for-the-badge&logo=NixOS&logoColor=white
[nix-url]: https://nixos.org/
[haskell-shield]: https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white
[haskell-url]: https://www.haskell.org/
[issues-shield]: https://img.shields.io/github/issues/mrcjkb/haskell-snippets.nvim.svg?style=for-the-badge
[issues-url]: https://github.com/mrcjkb/haskell-snippets.nvim/issues
[license-shield]: https://img.shields.io/github/license/mrcjkb/haskell-snippets.nvim.svg?style=for-the-badge
[license-url]: https://github.com/mrcjkb/haskell-snippets.nvim/blob/master/LICENSE
[ci-shield]: https://img.shields.io/github/actions/workflow/status/mrcjkb/haskell-snippets.nvim/nix-build.yml?style=for-the-badge
[ci-url]: https://github.com/mrcjkb/haskell-snippets.nvim/actions/workflows/nix-build.yml
