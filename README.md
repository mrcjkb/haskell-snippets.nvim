<!-- markdownlint-disable -->
<br />
<div align="center">
  <a href="https://github.com/mrcjkb/haskell-snippets.nvim">
    <img src="./logo.svg" alt="haskell-snippets.nvim">
  </a>
  <p align="center">
    <a href="https://github.com/mrcjkb/haskell-snippets.nvim/issues">Report Bug</a>
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
[![LuaRocks][luarocks-shield]][luarocks-url]

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
- A Haskell tree-sitter parser or
  [`nvim-treesitter`](https://github.com/nvim-treesitter/nvim-treesitter)
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

> [!NOTE]
>
> See also `:h haskell-snippets`

## Snippets

> [!NOTE]
>
> - The recording software [ttyrec](https://github.com/mjording/ttyrec)
>   sometimes has some visual glitches.
> - Many snippets provided by this plugin use [choice nodes](https://github.com/L3MON4D3/LuaSnip/blob/master/DOC.md#choicenode)
>   recursively.

### Pragmas

#### `haskell-snippets.pragmas.prag`

- Trigger: `prag`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/ced58a75-ce1a-4a74-8d1f-8d715d68d81a)

#### `haskell-snippets.pragmas.lang`

- Trigger: `lang`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/31a6f298-c2c3-4526-a1bf-430a91ee9911)

#### `haskell-snippets.pragmas.discover`

- Trigger: `discover`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/0a5cabc3-1e58-48ff-9ac0-4abea61484ee)

#### `haskell-snippets.pragmas.nowarn`

- Trigger: `nowarn`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/3ded002b-9f8f-4a9e-9d9d-587a9187af62)

### Module and imports

#### `haskell-snippets.module.mod`

- Trigger: `mod`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/173bae48-39ba-4cc8-a5aa-24a41fb34bef)

#### `haskell-snippets.module.qual`

- Trigger: `qual`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/c62eac49-e06c-4ed9-9a9c-b987c1ad498c)

#### `haskell-snippets.module.impc`

- Trigger: `impc`
- Requires a tree-sitter parser for Haskell.

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/6ffc5f2f-c734-4e48-ac13-99ba1f9a0b18)

#### `haskell-snippets.module.qualc`

- Trigger: `qualc`
- Requires a tree-sitter parser for Haskell.

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/43cef52f-eeda-48ad-bb2d-86241ac715b2)

### Data and typeclasses

#### `haskell-snippets.data.adt`

- Trigger: `adt`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/668115de-5a2c-4ed8-b962-badf39337d99)

#### `haskell-snippets.data.newtype`

- Trigger: `new`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/69632453-eae2-49a4-8f21-d13276510aba)

#### `haskell-snippets.data.rec`

- Trigger: `rec`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/dbe341b7-8af4-49c4-a41f-362341dd9248)

#### `haskell-snippets.data.cls`

- Trigger: `cls`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/1b386334-cf42-4989-a876-45b7fb529a66)

#### `haskell-snippets.data.ins`

- Trigger: `ins`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/60ab78d6-62f6-4b8a-97f3-6c211d4333dd)

#### `haskell-snippets.data.constraint`

- Trigger: `=>`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/018d0044-ae47-416d-9989-43faf3d1e2e1)

### Functions

#### `haskell-snippets.functions.fn`

- Trigger: `fn`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/d5d7f76a-0ba0-4696-825e-9908f32dbe84)

#### `haskell-snippets.functions.func`

- Trigger: `func`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/b399c7a7-684f-4af6-8ed9-bbf3fbd0119a)

#### `haskell-snippets.functions.lambda`

- Trigger: `\`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/113a9e36-01c3-4ff7-8686-3f8e9560b44a)

### Expressions

#### `haskell-snippets.expressions.if_expr`

- Trigger: `if`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/a4f67a0b-3151-41e3-819d-7815d4253479)

#### `haskell-snippets.expressions.if_expr_multiline`

- Trigger: `iff`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/e450802a-756f-4882-8125-05994e48457c)

- Trigger: `case`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/8d0fbfeb-00c2-4644-b7a4-9d5993a852ea)

#### `haskell-snippets.expressions.if_expr_multiway`

- Trigger: `ifmw`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/5932ae6b-8f02-43b1-b621-eb19268750c2)

#### `haskell-snippets.expressions.lambdacase`

- Trigger: `\case`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/629670c7-93a7-4c34-a431-9eee55092d6a)

### QuasiQuotes

#### `haskell-snippets.quasiquotes.qq`

- Trigger: `qq`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/e34f590f-8032-4769-98f0-119d6e972bc3)

#### `haskell-snippets.quasiquotes.sql`

- Trigger: `sql`

![tty](https://github.com/mrcjkb/haskell-snippets.nvim/assets/12857160/92b3ba4e-84dd-44ca-b20d-483acd04df6d)

## Contributing

All contributions are welcome!
See [CONTRIBUTING.md](./CONTRIBUTING.md).

## Recommendations

Here are some other plugins I recommend for Haskell development:

- [mrcjkb/haskell-tools.nvim](https://github.com/mrcjkb/haskell-tools.nvim):
  Toolset to improve the Haskell experience in Neovim.
- [neotest-haskell](https://github.com/MrcJkb/neotest-haskell):
  Interact with tests in neovim.
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
[luarocks-shield]: https://img.shields.io/luarocks/v/MrcJkb/haskell-snippets.nvim?logo=lua&color=purple&style=for-the-badge
[luarocks-url]: https://luarocks.org/modules/MrcJkb/haskell-snippets.nvim
