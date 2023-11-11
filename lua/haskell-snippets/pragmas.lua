---@mod haskell-snippets.pragmas

---@brief [[
--- Snippets related to pragmas
---@brief ]]

---@class PragmaSnippetCollection
---@field prag Snippet Compiler pragma
---@field lang Snippet Language pragma
---@field discover Snippet Hspec/Sydtest discover GHC option
---@field nowarn Snippet GHC option

---@type PragmaSnippetCollection
local pragmas = {
  all = {},
}

local ls = require('luasnip')
local s = ls.snippet
local sn = ls.snippet_node
local text = ls.text_node
local insert = ls.insert_node
local choice = ls.choice_node
local dynamic = ls.dynamic_node

local util = require('haskell-snippets.util')

pragmas.prag = s({
  trig = 'prag',
  dscr = 'Compiler pragma',
}, {
  text('{-# '),
  choice(1, {
    sn(nil, {
      insert(1),
    }),
    sn(nil, {
      text('LANGUAGE '),
      insert(1),
    }),
    sn(nil, {
      text('OPTIONS_GHC '),
      choice(1, {
        insert(1),
        sn(nil, {
          text('-Wno-'),
          choice(1, {
            insert(1),
            text('deprecations'),
          }),
        }),
      }),
    }),
    sn(nil, {
      text('OPTIONS_GHC -F -pgmF '),
      insert(1),
    }),
    sn(nil, {
      text('INLINE '),
      insert(1),
    }),
    sn(nil, {
      text('INLINABLE '),
      insert(1),
    }),
    sn(nil, {
      text('NOINLINE '),
      insert(1),
    }),
  }),
	text(' #-}'),
})
table.insert(pragmas.all, pragmas.prag)

pragmas.lang = s({
  trig = 'lang',
  dscr = 'LANGUAGE pragma',
}, {
  text('{-# LANGUAGE '),
  choice(1, {
    insert(1),
    text('ScopedTypeVariables'),
    text('RecordWildCards'),
    text('LambdaCase'),
    text('QuasiQuotes'),
    text('ViewPatterns'),
    text('DerivingVia'),
    text('DeriveAnyClass'),
    text('DeriveGeneric'),
    text('MultiParamTypeClasses'),
    text('TypeFamilies'),
    text('DataKinds'),
    text('OverloadedLists'),
  }),
  text(' #-}'),
})
table.insert(pragmas.all, pragmas.lang)

local function get_module_name()
  local module_name = util.lsp_get_module_name()
  if module_name then
    return sn(nil, {
      insert(1, module_name),
    })
  end
  return sn(nil, {
    insert(1, 'Spec'),
  })
end

pragmas.discover = s({
  trig = 'discover',
  dscr = 'hspec/sydtest discover GHC option',
}, {
  text('{-# OPTIONS_GHC -F -pgmF '),
  choice(1, {
    text('hspec'),
    text('sydtest'),
  }),
  text('-discover -optF --module-name='),
  dynamic(2, get_module_name),
  text(' #-}'),
})
table.insert(pragmas.all, pragmas.discover)

pragmas.nowarn = s({
  trig = 'nowarn',
  dscr = 'GHC option to disable warnings',
}, {
  text('{-# OPTIONS_GHC -fno-warn-'),
  choice(1, {
    text('deprecations'),
    text('orphans'),
    text('unused-binds'),
    text('unused-matches'),
    text('unused-imports'),
    text('incomplete-patterns'),
  }),
  text(' #-}'),
})
table.insert(pragmas.all, pragmas.nowarn)

return pragmas
