describe('Setup', function()
  it('Can setup luasnip', function()
    local ls = require('luasnip')
    ls.setup {
      history = true,
      update_events = 'TextChanged,TextChangedI',
      delete_check_events = 'TextChanged',
      ext_base_prio = 300,
      ft_func = require('luasnip.extras.filetype_functions').from_cursor_pos,
    }
    local haskell_snippets = require('haskell-snippets').all
    ls.add_snippets('haskell', haskell_snippets, { key = 'haskell' })
  end)
end)
