---@mod haskell-snippets

---@brief [[
--- Collection of LuaSnip snippets for Haskell.
---
--- To add the snippets to LuaSnip:
--->
---local ls = require('luasnip')
---ls.setup {
---  -- Add your LuaSnip config
---}
---local haskell_snippets = require('haskell-snippets').all
---ls.add_snippets('haskell', haskell_snippets, { key = 'haskell' })
---<
---@brief ]]

---@alias Snippet table

---@class HaskellSnippetCollection
local hs = {
  all = {},
}

--- Snippets related to pragmas
hs.pragmas = require('haskell-snippets.pragmas')
vim.list_extend(hs.all, hs.pragmas.all)

--- Snippets related to modules
hs.module = require('haskell-snippets.module')
vim.list_extend(hs.all, hs.module.all)

--- Snippets related to data
hs.data = require('haskell-snippets.data')
vim.list_extend(hs.all, hs.data.all)

--- Snippets related to functions
hs.functions = require('haskell-snippets.functions')
vim.list_extend(hs.all, hs.functions.all)

--- Snippets related to expressions
hs.expressions = require('haskell-snippets.expressions')
vim.list_extend(hs.all, hs.expressions.all)

--- Snippets related to quasiqotes
hs.quasiqotes = require('haskell-snippets.quasiquotes')
vim.list_extend(hs.all, hs.quasiqotes.all)

return hs
