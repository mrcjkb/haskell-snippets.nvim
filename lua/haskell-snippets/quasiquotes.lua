---@mod haskell-snippets.quasiqotes

---@brief [[

---WARNING: This is not part of the public API.
---Breaking changes to this module will not be reflected in the semantic versioning of this plugin.

--- Snippets related to quasiqotes
---@brief ]]

local quasiquotes = {
  all = {},
}

local ls = require('luasnip')
local s = ls.snippet
local text = ls.text_node
local insert = ls.insert_node
local dynamic = ls.dynamic_node

local util = require('haskell-snippets.util')

--- QuasiQuote
quasiquotes.qq = s({
  trig = 'qq',
  dscr = 'QuasiQuote',
}, {
  text('['),
  insert(1),
  text('|'),
  insert(2),
  text('|'),
  text(']'),
})
table.insert(quasiquotes.all, quasiquotes.qq)

--- postgres-simple [sql||] QuasiQuote
quasiquotes.sql = s({
  trig = 'sql',
  dscr = 'postgres-simple sql QuasiQuote',
}, {
  text('[sql|'),
  dynamic(1, util.indent_newline_insert()),
  dynamic(2, util.indent_newline_text('|]', false)),
})
table.insert(quasiquotes.all, quasiquotes.sql)

return quasiquotes
