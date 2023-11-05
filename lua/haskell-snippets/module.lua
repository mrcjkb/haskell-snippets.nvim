---@mod haskell-snippets.module

---@brief [[
--- Snippets related to modules
---@brief ]]

---@class ModuleSnippetCollection
---@field mod Snippet Module declaration
---@field qual Snippet Qualified import

---@type ModuleSnippetCollection
local module = {
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

local has_treesitter, parsers = pcall(require, 'nvim-treesitter.parsers')
local hs_lang = has_treesitter and parsers.ft_to_lang('haskell') or 'haskell'
local has_haskell_parser = pcall(vim.treesitter.get_string_parser, '', 'haskell')

local function get_module_name_node()
  local module_name = util.lsp_get_module_name()
  if module_name then
    return sn(nil, { text(module_name) })
  end
  return sn(nil, { insert(1) })
end

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

--- Parses without injections
local function fast_parse(lang_tree)
  if lang_tree._valid then
    return lang_tree._trees
  end
  local parser = lang_tree._parser
  local old_trees = lang_tree._trees
  return parser:parse(old_trees[1], lang_tree._source)
end

local function get_qualified_name_node(args)
  local node_ref = args[1]
  local module_name = node_ref and #node_ref > 0 and node_ref[1]
  if not module_name then
    return sn(nil, { text('') })
  end
  local import_stmt = 'import qualified ' .. module_name
  local choices = { insert(1) }
  if has_haskell_parser then
    local module_query = vim.treesitter.query.parse(hs_lang, '(module) @mod')
    local lang_tree = vim.treesitter.get_string_parser(import_stmt, hs_lang, { injections = { [hs_lang] = '' } })
    local root = fast_parse(lang_tree):root()
    ---@diagnostic disable-next-line
    for _, match in module_query:iter_matches(root, import_stmt) do
      for _, node in ipairs(match) do
        local txt = vim.treesitter.get_node_text(node, import_stmt)
        table.insert(choices, 1, text(txt:sub(1, 1)))
        table.insert(choices, 1, text(txt))
      end
    end
  end
  return sn(nil, {
    choice(1, choices),
  })
end

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

return module
