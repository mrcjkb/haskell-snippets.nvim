---@mod haskell-snippets.expressions

---@brief [[

---WARNING: This is not part of the public API.
---Breaking changes to this module will not be reflected in the semantic versioning of this plugin.

--- Snippets related to expressions
---@brief ]]

local expressions = {
  all = {},
}

local util = require('haskell-snippets.util')

local ls = require('luasnip')
local s = ls.snippet
local text = ls.text_node
local insert = ls.insert_node
local dynamic = ls.dynamic_node

--- if expression
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

--- if expression (multi-line)
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

--- case expression (pattern match)
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

--- lambda case (pattern match)
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
