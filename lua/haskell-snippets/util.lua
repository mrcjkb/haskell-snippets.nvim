---@mod haskell-snippets.util

---@brief [[

---WARNING: This is not part of the public API.
---Breaking changes to this module will not be reflected in the semantic versioning of this plugin.

--- Utility functions
---@brief ]]
local util = {}

local ls = require('luasnip')
local sn = ls.snippet_node
local text = ls.text_node
local insert = ls.insert_node

local function cur_buf_opt(name)
  ---@diagnostic disable-next-line
  return vim.api.nvim_buf_get_option(0, name)
end

function util.indent_str()
  if cur_buf_opt('expandtab') then
    local indent = cur_buf_opt('shiftwidth')
    if indent == 0 then
      indent = cur_buf_opt('tabstop')
    end
    return string.rep(' ', indent)
  end
  return '\t'
end

---@param mk_node function
---@param extra_indent boolean?
local function _indent_newline(mk_node, extra_indent, _, parent)
  extra_indent = extra_indent == nil or extra_indent
  local _, pos = pcall(function()
    -- XXX: Hack to work around below bug
    return parent:get_buf_position()
  end)
  -- FIXME: This prints an error
  -- local pos = parent:get_buf_position()
  local indent_count = pos[2]
  local indent_str = string.rep(' ', indent_count) .. (extra_indent and util.indent_str() or '')
  return mk_node(indent_str)
end

function util.indent_newline_text(txt, extra_indent)
  local function mk_node(indent_str)
    return sn(nil, { text { '', indent_str .. txt } })
  end
  return function(...)
    return _indent_newline(mk_node, extra_indent, ...)
  end
end

function util.indent_newline_insert(txt, extra_indent)
  local function mk_node(indent_str)
    return sn(nil, {
      text { '', indent_str },
      insert(1, txt),
    })
  end
  return function(...)
    return _indent_newline(mk_node, extra_indent, ...)
  end
end

---@diagnostic disable-next-line: deprecated
local get_clients = vim.lsp.get_clients or vim.lsp.get_active_clients

---@return string|nil
function util.lsp_get_module_name()
  if #get_clients { bufnr = 0 } > 0 then
    for _, lens in pairs(vim.lsp.codelens.get(0)) do
      -- Strings to match taken from the module name plugin:
      -- https://github.com/haskell/haskell-language-server/blob/f0c16469046bd554828ea057b5e1f047ad02348e/plugins/hls-module-name-plugin/src/Ide/Plugin/ModuleName.hs#L129-L136
      local name_module_decl_absent = lens.command.title:match('module (.*) where')
      local name_module_decl_present = lens.command.title:match('Set module name to (.*)')
      local name = name_module_decl_absent or name_module_decl_present
      if name then
        return name
      end
    end
  end
end

return util
