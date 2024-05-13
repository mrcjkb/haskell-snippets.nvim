---@mod haskell-snippets.module

---@brief [[
--- Snippets related to modules
---@brief ]]

---@class ModuleSnippetCollection
local module = {
  ---@type Snippet[] All module-related snippets
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
local func = ls.function_node

local has_treesitter, parsers = pcall(require, 'nvim-treesitter.parsers')
local hs_lang = has_treesitter and parsers.ft_to_lang('haskell') or 'haskell'
local has_haskell_parser = pcall(vim.treesitter.get_string_parser, '', 'haskell')

--- Parses without injections
local function fast_parse(lang_tree)
  if lang_tree._valid then
    return lang_tree._trees
  end
  local parser = lang_tree._parser
  local old_trees = lang_tree._trees
  return parser:parse(old_trees[1], lang_tree._source)
end

---@param apply fun(module_name: string):(string|nil) Callback to apply the module name to. If the callback returns, this function returns.
---@param content string The content to parse from
---@param query_string string The tree-sitter query string with a '@mod' capture (v0.21.0 rewrite)
---@param legacy_query_string string The legacy tree-sitter query string with a '@mod' capture
---@return string|nil
local function treesitter_module_name(apply, content, query_string, legacy_query_string)
  assert(has_haskell_parser, 'No tree-sitter parser for Haskell found.')
  local ok, module_query = pcall(vim.treesitter.query.parse, hs_lang, query_string)
  if not ok then
    module_query = vim.treesitter.query.parse(hs_lang, legacy_query_string)
  end
  local lang_tree = vim.treesitter.get_string_parser(content, hs_lang, { injections = { [hs_lang] = '' } })
  local root = fast_parse(lang_tree):root()
  ---@diagnostic disable-next-line
  for _, match in module_query:iter_matches(root, content) do
    for _, node in ipairs(match) do
      local txt = vim.print(vim.treesitter.get_node_text(node, content))
      local result = apply(txt)
      if result then
        return result
      end
    end
  end
end

---@return string | nil
local function get_buf_module_name(_)
  return vim.fn.fnamemodify(vim.api.nvim_buf_get_name(0), ':t:r')
end

local function get_module_name_node()
  local module_name = util.lsp_get_module_name() or get_buf_module_name()
  if module_name then
    return sn(nil, { text(module_name) })
  end
  return sn(nil, { insert(1) })
end

---@type Snippet Module declaration
module.mod = s({
  trig = 'mod',
  dscr = 'Module declaration',
}, {
  text('module '),
  dynamic(1, get_module_name_node),
  choice(2, {
    text(' () where'),
    sn(nil, {
      dynamic(1, function()
        return sn(nil, { text { '', util.indent_str() .. '( ' } })
      end),
      insert(2),
      dynamic(3, function()
        return sn(nil, { text { '', util.indent_str() .. ') where' } })
      end),
    }),
    sn(nil, {
      text { ' (main) where', '', 'main :: IO ()', 'main = ' },
      insert(1),
    }),
  }),
})
table.insert(module.all, module.mod)

local function get_qualified_name_node(args)
  local node_ref = args[1]
  local module_name = node_ref and #node_ref > 0 and node_ref[1]
  if not module_name then
    return sn(nil, { text('') })
  end
  local import_stmt = 'import qualified ' .. module_name
  local choices = { insert(1) }
  if has_haskell_parser then
    treesitter_module_name(function(mod)
      table.insert(choices, 1, text(mod:sub(1, 1)))
      table.insert(choices, 1, text(mod))
    end, import_stmt, '(module_id) @mod', '(module) @mod')
  end
  return sn(nil, {
    choice(1, choices),
  })
end

---@type Snippet Qualified import
module.qual = s({
  trig = 'qual',
  dscr = 'Qualified import',
}, {
  text('import qualified '),
  insert(1),
  text(' as '),
  dynamic(2, get_qualified_name_node, { 1 }),
})
table.insert(module.all, module.qual)

if has_haskell_parser then
  ---@type Snippet | nil Import (child module)
  module.impc = s({
    trig = 'impc',
    dscr = 'Import child module',
  }, {
    text('import '),
    func(get_buf_module_name, {}, {}),
    text('.'),
    choice(1, {
      text('Internal'),
      text('Types'),
      sn(nil, {
        insert(1),
      }),
    }),
  })
  table.insert(module.all, module.impc)

  ---@type Snippet | nil Qualified import (child module)
  module.qualc = s({
    trig = 'qualc',
    dscr = 'Qualified import of child module',
  }, {
    text('import qualified '),
    func(get_buf_module_name, {}, {}),
    text('.'),
    choice(1, {
      text('Internal'),
      text('Types'),
      sn(nil, {
        insert(1),
      }),
    }),
    text(' as '),
    dynamic(2, get_qualified_name_node, { 1 }),
  })
  table.insert(module.all, module.qualc)
end
return module
