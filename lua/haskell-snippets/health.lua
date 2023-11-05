local health = {}

local h = vim.health or require('health')
local start = h.start or h.report_start
local ok = h.ok or h.report_ok
local error = h.error or h.report_error
local warn = h.warn or h.report_warn

---@class ExternalDependency
---@field name string Name of the dependency
---@field get_binaries fun():string[]Function that returns the binaries to check for
---@field optional fun():boolean Function that returns whether the dependency is optional
---@field url string URL (markdown)
---@field info string Additional information
---@field extra_checks function|nil Optional extra checks to perform if the dependency is installed

---@type ExternalDependency[]
local external_dependencies = {
  {
    name = 'haskell-language-server',
    get_binaries = function()
      local _, HTConfig = pcall(require, 'haskell-tools.config.internal')
      local default = { 'haskell-language-server-wrapper', 'haskell-language-server' }
      if not HTConfig then
        return default
      end
      local Types = require('haskell-tools.types.internal')
      local cmd = Types.evaluate(HTConfig.hls.cmd)
      if not cmd or #cmd == 0 then
        return default
      end
      return { cmd[1] }
    end,
    optional = function()
      return true
    end,
    url = '[haskell-language-server](https://haskell-language-server.readthedocs.io)',
    info = 'Some snippets work better with LSP.',
  },
}

---@param dep ExternalDependency
---@return boolean is_installed
---@return string|nil version
local check_installed = function(dep)
  local binaries = dep.get_binaries()
  for _, binary in ipairs(binaries) do
    if vim.fn.executable(binary) == 1 then
      local handle = io.popen(binary .. ' --version')
      if handle then
        local binary_version, error_msg = handle:read('*a')
        handle:close()
        if error_msg then
          return true
        end
        return true, binary_version
      end
      return true
    end
  end
  return false
end

---@param dep ExternalDependency
local function check_external_dependency(dep)
  local installed, mb_version = check_installed(dep)
  if installed then
    local version = mb_version and mb_version:sub(0, mb_version:find('\n') - 1) or '(unknown version)'
    ok(('%s: found %s.'):format(dep.name, version))
    if dep.extra_checks then
      dep.extra_checks()
    end
    return
  end
  if dep.optional() then
    warn(([[
      %s: not found.
      Install %s for extended capabilities.
      %s
      ]]):format(dep.name, dep.url, dep.info))
  else
    error(([[
      %s: not found.
      haskell-snippets.nvim requires %s.
      %s
      ]]):format(dep.name, dep.url, dep.info))
  end
end

function health.check()
  start('Checking external dependencies')
  for _, dep in ipairs(external_dependencies) do
    check_external_dependency(dep)
  end

  start('Checking tree-sitter parsers')
  local success = pcall(vim.treesitter.get_string_parser, '', 'haskell')
  if not success then
    warn('The tree-sitter parser for Haskell is not installed.')
  else
    ok('The tree-sitter parser for Haskell is installed.')
  end
end

return health
