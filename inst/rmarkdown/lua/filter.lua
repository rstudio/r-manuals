-- for debugging purpose
local debug_mode = os.getenv("DEBUG_PANDOC_LUA") == "TRUE"
-- local debug_mode = TRUE
function print_debug(label,obj,iter)
    obj = obj or nil
    iter = iter or pairs
    label = label or ""
    label = "DEBUG (from custom-environment.lua): "..label
    if (debug_mode) then
        if not obj then
            print(label.." nil")
        elseif (type(obj) == "string") then
            print(label.." "..obj)
        elseif type(obj) == "table" then
            for k,v in iter(obj) do
                print(label.."id:"..k.. " val:"..v)
            end
        end
    end
    return nil
end

-- Determines if item is in a list (table) of items
local in_list = function(item, table)
  local valid = {}
  for i = 1, #table do valid[table[i]] = true end
  if valid[item] then return true else return false end
end


Blocks = function(blocks)
  for i = #blocks-1, 1, -1 do
    -- Add Blockquotes use to border some codeblock inside div with class
    -- so that we can target using CSS
    if blocks[i] and blocks[i].t == "BlockQuote" then
      if blocks[i].content[1] and blocks[i].content[1].t == "Table" then
        blocks[i] = pandoc.Div(blocks[i], {class = "blockquotes-block"})
      end
    end
  end
  return blocks
end

Span = function(el)
  -- Ensure empty spans show up in 
  if pandoc.utils.stringify(el.content) == "" and el.identifier ~= nil then
    el.content = {""}
  end
  return el
end

Div = function(el)
  local classes = el.classes
  if in_list("header", classes) or
     in_list("contents", classes) or
     in_list("contents-heading", classes) or
     in_list("Contents_element", classes) then
    return pandoc.Plain("")
  elseif in_list("top", classes) or
         in_list("example", classes) or
         in_list("section", classes) or
         in_list("chapter", classes) then
    return el.content
  elseif el.classes[1] == "footnote" then
    local new = pandoc.walk_block(el, {
      Para = function(el)
        table.insert(el.content, 1, pandoc.Str("    "))
        return el
      end,
      Header = function(el)
        return {el.content}
      end
    })
    return new.content
  end
end

Header = function(el)
  if el.level <4 then
    el.identifier = ""
    return el
  end
end

HorizontalRule = function(el)
  return pandoc.Plain("")
end

-- loop over words to find a match
local scan = function(text, words)
  for i = 1, #words do
    if text:match(words[i]) then return true end
  end
  return false
end

local split = function(s, delim)
    local res = {}
    for m in s:gmatch(delim) do
        table.insert(res, m)
    end
    return res;
end

local fix_encoded_char = function(s)
  s = string.gsub(s, "_002b", "")
  s = string.gsub(s, "_002e", ".")
  s = string.gsub(s, "_002d", "-")
  s = string.gsub(s, "_005f", "_")
  return s
end

Link = function(el)
  if not scan(el.target, {"^http"}) and scan(el.target, {"#"}) then
    if el.target:sub(1,1) == "#" then
      -- don't process footnotes link
      if el.target:sub(2,4) ~= "DOC" and el.target:sub(2,4) ~= "FOO" then
        el.target = fix_encoded_char(pandoc.text.lower(el.target))
      end
    else
      local r = split(el.target, "[^#]*")
      el.target = fix_encoded_char(r[1]) .. '#' .. fix_encoded_char(pandoc.text.lower(r[2]))
    end
  end
  return(el)
end

CodeBlock = function(el)

  local t = el.text

  -- remove empty code block
  if (t == '') then return(pandoc.Null()) end

  -- R documentation (use latex)
  if scan(t, {
    "\\name%{", "\\concept%{", "\\inputencoding%{", "\\ifelse%{", "\\if%{",
    "\\tabular%{", "\\enumerate%{", "\\S4method%{", "\\item%{", "\\details%{", "\\Sexpr%{",
    "\\deqn%{", "\\section%{", "\\note%{", "\\PR%{"
  }) then
    el.classes[1] = "latex"

  -- C
  elseif scan(t, {
    "#if", "#define", "#include", "static%s", "PKG_", " void%s", "void %*", "type%*",
     "char %*", "double%s", "ISNA", "LIBRARY", "RNGstate%(%)", "SEXP", "*dll", "int *",
     "#pragma", "PROTECT%{"
    }) then
    el.classes[1] = "c"

  -- Defaut
  elseif scan(t, {
    "Package: ", "License:", "GPL%-2", "SystemRequirements", "Warning:",
    "LinkingTo", "tools::"
  }) then
    el.classes[1] = "default"

  -- R
  elseif scan(t, {
    "<%-", "paste0", "S3method", "useDynLib",
    "importFrom%(", "export%(", "%.Call",
    "import%(", "importClassesFrom", "exportPattern",
    "library%(", "requireNamespace", "tools::", "gettext", "lib.loc",
    "?library", "options%(", "system.time%(",
    "View%("
  }) then
    el.classes[1] = "r"

  -- bash
  elseif scan(t, {
    "sudo%s", "tar%s", "touch%s", "#!/", "R CMD", "dnl%s"
  }) then
    el.classes[1] = "bash"

  -- makefile
  elseif scan(t, {
    "%.PHONY", "all:%s", "all%.f", "CXX", "%-dPDF", "%$%(MAKE%)",
    "%$%(R_HOME%)", "%$%{R_HOME", "%.o%s", ":%s%$",
  }) then
    el.classes[1] = "makefile"


  -- latex
  elseif scan(t, {"%%\\Vignette", "\\usepackage", "%%\\Sweave"}) then
    el.classes[1] = "latex"

  -- uncategorized
  else
    el.classes[1] = "R"
  end
  return el
end
