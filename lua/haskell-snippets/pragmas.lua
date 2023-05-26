---@mod haskell-snippets.pragmas

---@brief [[

---WARNING: This is not part of the public API.
---Breaking changes to this module will not be reflected in the semantic versioning of this plugin.

--- Snippets related to pragmas
---@brief ]]

local pragmas = {
  all = {},
}

local ls = require('luasnip')
local s = ls.snippet
local sn = ls.snippet_node
local text = ls.text_node
local insert = ls.insert_node
local choice = ls.choice_node

--- Compiler pragma
pragmas.prag = s({
  trig = 'prag',
  dscr = 'Compiler pragma',
}, {
  text('{-# '),
  choice(1, {
    sn(nil, {
      insert(1),
      text(' #-}'),
    }),
    sn(nil, {
      text('LANGUAGE '),
      insert(1),
      text(' #-}'),
    }),
    sn(nil, {
      text('OPTIONS_GHC '),
      insert(1),
      text(' #-}'),
    }),
    sn(nil, {
      text('OPTIONS_GHC -F -pgmF '),
      insert(1),
      text(' #-}'),
    }),
    sn(nil, {
      text('INLINE '),
      insert(1),
      text(' #-}'),
    }),
    sn(nil, {
      text('INLINABLE '),
      insert(1),
      text(' #-}'),
    }),
    sn(nil, {
      text('NOINLINE '),
      insert(1),
      text(' #-}'),
    }),
  }),
})
table.insert(pragmas.all, pragmas.prag)

--- Language pragma
pragmas.lang = s({
  trig = 'lang',
  dscr = 'LANGUAGE pragma',
}, {
  text('{-# LANGUAGE '),
  choice(1, {
    insert(1),
    text('ScopedTypeVariables'),
    text('RecordWildCards'),
    text('LamdaCase'),
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

--- Hspec/Sydtest discover GHC option
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
  insert(2, 'Spec'),
  text(' #-}'),
})
table.insert(pragmas.all, pragmas.discover)

--- nowarn GHC option
pragmas.nowarn = s({
  trig = 'nowarn',
  dscr = 'GHC option to disable warnings',
}, {
  text('{-# OPTIONS_GHC -fno-warn-'),
  choice(1, {
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
