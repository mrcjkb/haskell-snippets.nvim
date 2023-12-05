---@mod haskell-snippets.function

---@brief [[
--- Snippets related to functions
---@brief ]]

---@class FunctionSnippetCollection
local functions = {
  ---@type Snippet[] All function-related snippets
  all = {},
}

local util = require('haskell-snippets.util')

local ls = require('luasnip')
local s = ls.snippet
local sn = ls.snippet_node
local text = ls.text_node
local insert = ls.insert_node
local choice = ls.choice_node
local dynamic = ls.dynamic_node

---@type function
local build_function_snippet
build_function_snippet = function(multiline, args, _, _, user_args)
  if not user_args then
    local name_arg = args[1]
    local function_name = name_arg and #name_arg > 0 and name_arg[1] or ''
    user_args = {
      name = function_name,
      arg_count = 0,
    }
  end
  table.insert(user_args, args[1])
  local choices = {}
  local end_snip = {
    insert(1),
    text { '', user_args.name },
  }
  local idx = 2
  if user_args.arg_count > 0 then
    for _ = 1, user_args.arg_count do
      table.insert(end_snip, text(' '))
      table.insert(end_snip, insert(idx, '_'))
      idx = idx + 1
    end
  end
  table.insert(end_snip, text(' = '))
  table.insert(end_snip, insert(idx, 'undefined'))
  table.insert(choices, sn(nil, end_snip))
  local function wrapper(a, p, os)
    -- XXX: For some reason, user_args is not passed down.
    local updated_user_args = {
      name = user_args.name,
      arg_count = user_args.arg_count + 1,
    }
    return build_function_snippet(multiline, a, p, os, updated_user_args)
  end
  table.insert(
    choices,
    sn(nil, {
      insert(1),
      multiline and text { '', util.indent_str() .. '-> ' } or text(' -> '),
      dynamic(2, wrapper, nil),
    })
  )
  return sn(nil, {
    choice(1, choices),
  })
end

local function build_single_line_function_snippet(...)
  return build_function_snippet(false, ...)
end

local function build_multi_line_function_snippet(...)
  return build_function_snippet(true, ...)
end

---@type Snippet Function and type signature
functions.fn = s({
  trig = 'fn',
  dscr = 'Function and type signature',
}, {
  insert(1, 'someFunc'),
  text(' :: '),
  dynamic(2, build_single_line_function_snippet, { 1 }),
})
table.insert(functions.all, functions.fn)

---@type Snippet Function and type signature (multi-line)
functions.func = s({
  trig = 'func',
  dscr = 'Function and type signature (multi-line)',
}, {
  insert(1, 'someFunc'),
  dynamic(2, function()
    return sn(nil, { text { '', util.indent_str() .. ':: ' } })
  end),
  dynamic(3, build_multi_line_function_snippet, { 1 }),
})
table.insert(functions.all, functions.func)

---@type Snippet Lambda
functions.lambda = s({
  trig = '\\',
  dscr = 'Lambda',
}, {
  text('\\'),
  insert(1, '_'),
  text(' -> '),
  insert(2),
})
table.insert(functions.all, functions.lambda)

return functions
