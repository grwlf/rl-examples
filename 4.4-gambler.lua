N = 99
Eps = 0.01
Prob = 0.6

Plotter = require 'plotter'

-- REINFORCEMENT
function evaluate_policy(V, P)
   local Delta
   local steps = 0
   repeat
      Delta = 0
      for s=1,N do
         local bet = P[s] -- policy is deterministic
         local v = value(V,s,bet)
         Delta = math.max(Delta, math.abs(v - V[s]))
         V[s] = v
      end
      --      Renderer.updateV(V)
--      Renderer.draw()
      steps = steps + 1
   until Delta < Eps
   return V
end


function value(V, s, bet)
   local v0,v1
   if s-bet <= 0 then v0 = 0 else v0 = V[s-bet] end
   if s+bet > N then v1 = 1 else v1 = V[s+bet] end
   local v = Prob * v1 + (1-Prob) * v0
   print(v, s,bet,v0,v1)

   return v
end

function improve_policy(V, P)
   local changed = false

   for s=1,N do
      local best_a
      local best_v = -10e9

      for b=0,math.min(s, N+1 - s) do

         v = value(V,s, b)
         if v > best_v then
            best_a = b
            best_v = v
         end
      end
      if not (P[s] == best_a) then
         changed = true
         P[s] = best_a
      end
   end

   return P, changed
end

function recursive_policy_improvement()
   V = zeroTable()
   P = zeroTable()

   local changed
   repeat
      evaluate_policy(V, P)
      P, changed = improve_policy(V, P)
      Plotter.plot_table(V)
   until not changed
   return V, P
end


function zeroTable()
   local res = {}
   for i=0,N+1 do
      res[i] = 0
   end
   return res
end

recursive_policy_improvement()
