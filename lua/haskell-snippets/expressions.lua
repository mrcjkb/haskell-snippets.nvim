---@mod haskell-snippets.expressions

---@brief [[
--- Snippets related to expressions
---@brief ]]

---@class ExpressionSnippetCollection
local expressions = {
  ---@type Snippet[] All expression-related snippets
  all = {},
}

local util = require('haskell-snippets.util')

local ls = require('luasnip')
local s = ls.snippet
local text = ls.text_node
local insert = ls.insert_node
local dynamic = ls.dynamic_node

---@type Snippet if expression
expressions.if_expr = s({
  trig = 'if',
  dscr = 'If expression (single line)',
}, {
  text('if '),
  insert(1),
  text(' then '),
  insert(2),
  text(' else '),
  insert(3),
})
table.insert(expressions.all, expressions.if_expr)

---@type Snippet if expression (multi-line)
expressions.if_expr_multiline = s({
  trig = 'iff',
  dscr = 'If expression (multi lines)',
}, {
  text('if '),
  insert(1),
  dynamic(2, util.indent_newline_text('then ')),
  insert(3),
  dynamic(4, util.indent_newline_text('else ')),
  insert(5),
})
table.insert(expressions.all, expressions.if_expr_multiline)

---@type Snippet if expression (multi-way)
expressions.if_expr_multiway = s({
  trig = 'ifmw',
  dscr = 'If expression (multi-way)',
}, {
  text('if '),
  dynamic(1, util.indent_newline_text('| ')),
  insert(2),
  text(' -> '),
  insert(3),
  dynamic(4, util.indent_newline_text('| ')),
  insert(5),
  text(' -> '),
  insert(6),
  dynamic(7, util.indent_newline_text('| otherwise -> ')),
  insert(8),
})
table.insert(expressions.all, expressions.if_expr_multiway)

---@type Snippet case expression (pattern match)
expressions.case = s({
  trig = 'case',
  dscr = 'Case expression (pattern match)',
}, {
  text('case '),
  insert(1),
  text(' of'),
  dynamic(2, util.indent_newline_insert('_')),
  text(' -> '),
  insert(3),
  dynamic(4, util.indent_newline_insert('_')),
  text(' -> '),
  insert(5),
})
table.insert(expressions.all, expressions.case)

---@type Snippet lambda case (pattern match)
expressions.lambdacase = s({
  trig = '\\case',
  dscr = 'Lambda (pattern match)',
}, {
  text('\\case'),
  dynamic(1, util.indent_newline_insert('_')),
  text(' -> '),
  insert(2),
  dynamic(3, util.indent_newline_insert('_')),
  text(' -> '),
  insert(4),
})
table.insert(expressions.all, expressions.lambdacase)

return expressions
