---@mod haskell-snippets.pragmas

---@brief [[
--- Snippets related to pragmas
---@brief ]]

---@class PragmaSnippetCollection
local pragmas = {
  ---@type Snippet[] All pragma-related snippets
  all = {},
}

--- List of language extensions fetched from
--- https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/table.html as
--- of 2023-11-11.
---
---@type string[]
local language_extensions = {
  'AllowAmbiguousTypes',
  'ApplicativeDo',
  'Arrows',
  'BangPatterns',
  'BinaryLiterals',
  'BlockArguments',
  'CApiFFI',
  'ConstrainedClassMethods',
  'ConstraintKinds',
  'CPP',
  'CUSKs',
  'DataKinds',
  'DatatypeContexts',
  'DeepSubsumption',
  'DefaultSignatures',
  'DeriveAnyClass',
  'DeriveDataTypeable',
  'DeriveFoldable',
  'DeriveFunctor',
  'DeriveGeneric',
  'DeriveLift',
  'DeriveTraversable',
  'DerivingStrategies',
  'DerivingVia',
  'DisambiguateRecordFields',
  'DuplicateRecordFields',
  'EmptyCase',
  'EmptyDataDecls',
  'EmptyDataDeriving',
  'ExistentialQuantification',
  'ExplicitForAll',
  'ExplicitNamespaces',
  'ExtendedDefaultRules',
  'ExtendedLiterals',
  'FieldSelectors',
  'FlexibleContexts',
  'FlexibleInstances',
  'ForeignFunctionInterface',
  'FunctionalDependencies',
  'GADTs',
  'GADTSyntax',
  'GeneralisedNewtypeDeriving',
  'GHC2021',
  'GHCForeignImportPrim',
  'Haskell2010',
  'Haskell98',
  'HexFloatLiterals',
  'ImplicitParams',
  'ImplicitPrelude',
  'ImportQualifiedPost',
  'ImpredicativeTypes',
  'IncoherentInstances',
  'InstanceSigs',
  'InterruptibleFFI',
  'KindSignatures',
  'LambdaCase',
  'LexicalNegation',
  'LiberalTypeSynonyms',
  'LinearTypes',
  'MagicHash',
  'MonadComprehensions',
  'MonoLocalBinds',
  'MonomorphismRestriction',
  'MultiParamTypeClasses',
  'MultiWayIf',
  'NamedFieldPuns',
  'NamedWildCards',
  'NegativeLiterals',
  'NondecreasingIndentation',
  'NPlusKPatterns',
  'NullaryTypeClasses',
  'NumDecimals',
  'NumericUnderscores',
  'OverlappingInstances',
  'OverloadedLabels',
  'OverloadedLists',
  'OverloadedRecordDot',
  'OverloadedRecordUpdate',
  'OverloadedStrings',
  'PackageImports',
  'ParallelListComp',
  'PartialTypeSignatures',
  'PatternGuards',
  'PatternSynonyms',
  'PolyKinds',
  'PostfixOperators',
  'QualifiedDo',
  'QuantifiedConstraints',
  'QuasiQuotes',
  'Rank2Types',
  'RankNTypes',
  'RebindableSyntax',
  'RecordWildCards',
  'RecursiveDo',
  'RoleAnnotations',
  'Safe',
  'ScopedTypeVariables',
  'StandaloneDeriving',
  'StandaloneKindSignatures',
  'StarIsType',
  'StaticPointers',
  'Strict',
  'StrictData',
  'TemplateHaskell',
  'TemplateHaskellQuotes',
  'TraditionalRecordSyntax',
  'TransformListComp',
  'Trustworthy',
  'TupleSections',
  'TypeAbstractions',
  'TypeApplications',
  'TypeData',
  'TypeFamilies',
  'TypeFamilyDependencies',
  'TypeInType',
  'TypeOperators',
  'TypeSynonymInstances',
  'UnboxedSums',
  'UnboxedTuples',
  'UndecidableInstances',
  'UndecidableSuperClasses',
  'UnicodeSyntax',
  'UnliftedDatatypes',
  'UnliftedFFITypes',
  'UnliftedNewtypes',
  'Unsafe',
  'ViewPatterns',
}

local ls = require('luasnip')
local s = ls.snippet
local sn = ls.snippet_node
local text = ls.text_node
local insert = ls.insert_node
local choice = ls.choice_node
local dynamic = ls.dynamic_node

local util = require('haskell-snippets.util')

-- This needs to be a function, because LuaSnip's functions modify the input
-- structures in place. We need to ensure that every time we use this
-- structure, we have a fresh copy.
local function language_extension_interior_snippet()
  return sn(1, {
    text('LANGUAGE '),
    choice(1, vim.list_extend({ insert(1) }, vim.tbl_map(text, language_extensions))),
  })
end

---@type Snippet Compiler pragma
pragmas.prag = s({
  trig = 'prag',
  dscr = 'Compiler pragma',
}, {
  text('{-# '),
  choice(1, {
    sn(nil, {
      insert(1),
    }),
    language_extension_interior_snippet(),
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

---@type Snippet Language pragma
pragmas.lang = s({
  trig = 'lang',
  dscr = 'LANGUAGE pragma',
}, {
  text('{-# '),
  language_extension_interior_snippet(),
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

---@type Snippet Hspec/Sydtest discover GHC option
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

---@type Snippet GHC option
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
