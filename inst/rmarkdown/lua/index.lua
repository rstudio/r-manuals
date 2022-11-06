local vars = {}

function get_vars (meta)
  for k, v in pairs(meta) do
    if pandoc.utils.type(v) == 'Inlines' and k == "title" then
      vars[k] = pandoc.utils.stringify(v)
    end
  end
  -- print(pandoc.utils.stringify(vars["title"]))
end

Header = function(el)
  return pandoc.Plain("")
end

Para = function(el)
  if pandoc.utils.stringify(el) == vars["title"] then
    return {}
  end
end


Pandoc = function(doc)
  local hblocks = {}
  table.insert(hblocks, pandoc.Header(1, vars["title"] .. ' {.unnumbered}'))
  for i, el in pairs(doc.blocks) do
      table.insert(hblocks, el)
  end
  return pandoc.Pandoc(hblocks, doc.meta)
end


local replace = function(el)
  if vars[el.text] then
    return pandoc.Span(vars[el.text])
  else
    return el
  end
end

return {
  {Meta = get_vars},
  {Header = Header},
  {Para = Para},
  {Pandoc = Pandoc}
}
