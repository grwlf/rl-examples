local M = {}

local function indexes(tab)
   local min = 10e9
   local max = -10e9
   for i,v in pairs(tab) do
      if min > i then min = i end
      if max < i then max = i end
   end
   return min, max
end

function M.plot_table(tab)
   local min, max = indexes(tab)

   local gp = require 'gnuplot'

   if type(tab[min]) == "number" then
      local ten = torch.Tensor(max - min + 1)
      local i=1
      for k=min,max do
         ten[i] = tab[k]
         i = i + 1
      end
      gp.plot(ten)
   elseif type(tab[min]) == "table" then
      local min2, max2 = indexes(tab[min])

      local ten = torch.Tensor(max - min + 1, max2-min2+1)
      local i=1
      for k=min,max do
         local j = 1
         for l = min2,max2 do
            ten[i][j] = tab[k][l]
            j = j + 1
         end
         i = i + 1
      end
      gp.imagesc(ten)
   else error("unknown elt")
   end
end

return M
