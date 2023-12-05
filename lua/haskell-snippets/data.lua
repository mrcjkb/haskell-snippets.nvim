---@mod haskell-snippets.datadat

---@brief [[
--- Snippets related to data
---@brief ]]

---@class DataSnippetCollection
local data = {
  ---@type Snippet[] All data-related snippets
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
local adt_constructor_choice
adt_constructor_choice = function()
  return sn(nil, {
    choice(1, {
      text(''),
      sn(nil, {
        dynamic(1, function()
          return sn(nil, { text { '', util.indent_str() .. '| ' } })
        end),
        insert(2, 'Constructor'),
        dynamic(3, adt_constructor_choice),
      }),
    }),
  })
end

---@type Snippet Algebraic data type
data.adt = s({
  trig = 'adt',
  dscr = 'Algebraic data type',
}, {
  text('data '),
  insert(1, 'Type'),
  choice(2, {
    dynamic(1, function()
      return sn(nil, { text { '', util.indent_str() .. '= ' } })
    end),
    sn(nil, {
      text(' '),
      insert(1, 'a'),
      dynamic(2, function()
        return sn(nil, { text { '', util.indent_str() .. '= ' } })
      end),
    }),
  }),
  insert(3, 'Constructor'),
  dynamic(4, function()
    return sn(nil, { text { '', util.indent_str() .. '| ' } })
  end),
  insert(5, 'Constructor'),
  dynamic(6, adt_constructor_choice),
})
table.insert(data.all, data.adt)

---@type Snippet newtype
data.newtype = s({
  trig = 'new',
  dscr = 'newtype',
}, {
  text('newtype '),
  insert(1, 'Type'),
  choice(2, {
    text(' = '),
    sn(nil, {
      text(' '),
      insert(1, 'a'),
      text(' = '),
    }),
  }),
  insert(3, 'Constructor'),
  text(' '),
  insert(4, 'Int'),
})
table.insert(data.all, data.newtype)

---@type function
local record_field_choice

record_field_choice = function()
  return sn(nil, {
    choice(1, {
      sn(nil, {
        insert(1),
        dynamic(2, function()
          return sn(nil, { text { '', util.indent_str() .. '}' } })
        end),
      }),
      sn(nil, {
        insert(1),
        dynamic(2, function()
          return sn(nil, { text { '', util.indent_str() .. ', ' } })
        end),
        insert(3),
        text(' :: '),
        dynamic(4, record_field_choice),
      }),
    }),
  })
end

---@type Snippet Record
data.rec = s({
  trig = 'rec',
  dscr = 'Record',
}, {
  text('data '),
  insert(1, 'Type'),
  choice(2, {
    dynamic(1, function()
      return sn(nil, { text { '', util.indent_str() .. '= ' } })
    end),
    sn(nil, {
      text(' '),
      insert(1, 'a'),
      dynamic(2, function()
        return sn(nil, { text { '', util.indent_str() .. '= ' } })
      end),
    }),
  }),
  insert(3, 'Constructor'),
  dynamic(4, function()
    return sn(nil, { text { '', util.indent_str() .. '{ ' } })
  end),
  insert(5),
  text(' :: '),
  dynamic(6, record_field_choice),
  choice(7, {
    text(''),
    sn(nil, {
      dynamic(1, function()
        return sn(nil, { text { '', util.indent_str() .. 'deriving (' } })
      end),
      insert(2),
      text(')'),
    }),
    sn(nil, {
      dynamic(1, function()
        return sn(nil, { text { '', util.indent_str() .. 'deriving ' } })
      end),
      insert(2),
    }),
  }),
})
table.insert(data.all, data.rec)

---@type Snippet Typeclass
data.cls = s({
  trig = 'cls',
  dscr = 'Typeclass',
}, {
  text('class '),
  insert(1),
  dynamic(2, function()
    return sn(nil, { text { ' where', util.indent_str() } })
  end),
  insert(3),
})
table.insert(data.all, data.cls)

---@type Snippet Typeclass instance
data.ins = s({
  trig = 'ins',
  dscr = 'Typeclass instance',
}, {
  text('instance '),
  insert(1, 'Class'),
  text(' '),
  insert(2, 'Type'),
  dynamic(3, function()
    return sn(nil, { text { ' where', util.indent_str() } })
  end),
  insert(4),
})
table.insert(data.all, data.ins)

---@type Snippet Typeclass constraint
data.constraint = s({
  trig = '=>',
  descr = 'Typeclass constraint',
}, {
  insert(1, 'Class'),
  text(' '),
  insert(2, 'a'),
  text(' => '),
  insert(3),
})
table.insert(data.all, data.constraint)
return data
