N = 30
NA = 4
eps = 0.1
gamma = 0.90

-- RENDERING
local gp = require 'gnuplot'
Renderer = { P = nil, V = nil, P_num = 0, V_num = 0 }

function Renderer.init(draw)
   if not draw then
      gp.raw("set terminal gif animate delay 5 size 1200,700")
      gp.raw('set output "animate.gif"')
   end
end

function Renderer.updateV(v)
   Renderer.V = v
   Renderer.V_num = Renderer.V_num + 1
end

function Renderer.updateP(p)
   Renderer.P = p
   Renderer.P_num = Renderer.P_num + 1
   Renderer.V_num = 0
end

function Renderer.draw()
   gp.raw("set multiplot layout 1,2")
   gp.raw(string.format("set title 'value map %d'", Renderer.V_num))
   gnuplot.imagesc(Renderer.V,'color')
   gp.raw(string.format("set title 'policy map %d'", Renderer.P_num))
   gnuplot.imagesc(Renderer.P,'color')
   gp.raw("unset multiplot")
end

-- REINFORCEMENT
function evaluate_policy(V, trans, policy, reward)
   local Delta
   local steps = 0
   repeat
      Delta = 0
      for i=1,N do
         for j=1,N do
            local v = 0
            for a=1,NA do
               local i2,j2 = trans(i,j, a)
               local R = reward(i,j, i2,j2, a) -- skipped i,j,a
               v = v + policy(i,j, a) * (R + gamma * V[i2][j2])
            end
            Delta = math.max(Delta, math.abs(v - V[i][j]))
            V[i][j] = v
         end
      end
      Renderer.updateV(V)
      Renderer.draw()
      steps = steps + 1

   until Delta < eps
   io.write(string.format("found in %d steps\n", steps))
   return V
end

function improve_policy(V)
   local P = torch.zeros(N,N)

   for i=1,N do
      for j=1,N do
         local max
         local max_i = 0
         for a=1,NA do
            local i2,j2 = trans(i,j, a)
            local R = reward(i,j, i2,j2, a)
            local v = R + gamma * V[i2][j2]

            if max_i == 0 or max < v then
               max_i = a
               max = v
            end
         end
         P[i][j] = max_i
      end
   end
   return P
end

function is_terminal(i,j)
   return i == 1 and j == 1 or i == N and j == N
end

function is_offboard(i,j)
   return i < 1 or i > N or j < 1 or j > N
end

function trans(i,j,a)
   local i2,j2
   if is_terminal(i,j) then i2,j2 = i,j
   elseif a == 1 then i2,j2 = i-1,j
   elseif a == 2 then i2,j2 = i,j-1
   elseif a == 3 then i2,j2 = i+1,j
   elseif a == 4 then i2,j2 = i,j+1
   else error "unknown action"
   end
   if is_offboard(i2,j2) then return i,j
   else return i2,j2
end
end

function round(v)
   if math.abs(v - math.floor(v)) < 0.5 then return math.floor(v) else return math.ceil(v) end
end

function on_centered_square(i,j, r)
   local ci = round(N / 2)
   local cj = round(N / 2)

   return math.abs(i - ci) == r and math.abs(j - cj) <= r or
          math.abs(i - ci) <= r and math.abs(j - cj) == r
end

function reward(i,j, i2,j2, a)
   if is_terminal(i2,j2) then return 0 end

   local r1 = 5
   local r2 = 8

   if on_centered_square(i2, j2, 5) and not (i2 == round(N / 2) and i2 > j2) then
      return -100
   end

   if on_centered_square(i2, j2, 8) and not (i2 == round(N / 2) and i2 < j2) then
      return -100
   end

   if on_centered_square(i2, j2, 12) and not (j2 == round(N / 2) and i2 < j2) then
      return -100
   end


   if i == i2 and j == j2 then return -3 end

   return -1

end

function random_P()
   local P = torch.Tensor(N,N)
   P:apply(function(v) return torch.random(1,NA) end)
   return P
end

function recursive_policy_improvement(draw)

   local V = torch.zeros(N,N)
   local old_P = random_P()
   local P = old_P
   local stale
   local i = 1

   Renderer.updateV(V)
   Renderer.updateP(P)
   repeat
      local policy = function(i,j,a)
         if P[i][j] == a then return 1
         else return 0
         end
      end

      V = evaluate_policy(V, trans, policy, reward)
      P = improve_policy(V)
      stale = torch.all(torch.eq(old_P,P))
      old_P = P

      Renderer.updateP(P)
      Renderer.draw()

      i = i + 1

   until stale
   io.write(string.format("improved in %d steps", i))
   return P, vs
end

Renderer.init()
p,v = recursive_policy_improvement()

print(p)
print(v)
