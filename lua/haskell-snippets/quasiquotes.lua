---@mod haskell-snippets.quasiqotes

---@brief [[
--- Snippets related to quasiqotes
---@brief ]]

---@class QuasiQuoteSnippetCollection
---@field qq Snippet QuasiQuote
---@field sql Snippet postgres-simple [sql||] QuasiQuote

---@type QuasiQuoteSnippetCollection
local quasiquotes = {
  all = {},
}

local ls = require('luasnip')
local s = ls.snippet
local text = ls.text_node
local insert = ls.insert_node
local dynamic = ls.dynamic_node

local util = require('haskell-snippets.util')

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
