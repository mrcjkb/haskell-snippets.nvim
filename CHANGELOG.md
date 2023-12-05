<!-- markdownlint-disable -->
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.4.1] - 2023-12-05

### Fixed

- Luarocks package: Add `LuaSnip` dependency.

## [1.4.0] - 2023-12-05

### Added

- Module imports:
  - `qualc` snippet for qualified import of child modules.
  - `impc` snippet for unqualified import of child modules.

## [1.3.0] - 2023-12-05

### Added

- Expressions: Add multi-way if snippet (`ifmw`) [[#16](https://github.com/mrcjkb/haskell-snippets.nvim/pull/16)].
  Thanks [@gregorias](https://github.com/gregorias)!

## [1.2.0] - 2023-11-12

### Added

- Pragmas: Add all language extensions as choice options [[#12](https://github.com/mrcjkb/haskell-snippets.nvim/pull/12#event-10934080925)].
  Thanks [@gregorias](https://github.com/gregorias)!
- Health checks.
- Use tree-sitter if the parser for Haskell is installed,
  without requiring the nvim-treesitter plugin.

## [1.1.0] - 2023-11-05

### Added

- Add `no-deprecations` choices [[#11](https://github.com/mrcjkb/haskell-snippets.nvim/pull/11)].
  Thanks [@gregorias](https://github.com/gregorias)!

### Fixed

- Typo in `LambdaCase` pragma snippet

## [1.0.0] - 2023-05-28

### Added

- Initial release
