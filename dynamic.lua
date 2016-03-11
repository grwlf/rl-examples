N = 4
NA = 4
eps = 0.001
gamma = 0.9

function evaluate_states(trans, policy, reward)
   local V = torch.zeros(N,N)
   local Delta
   local steps = 0
   repeat
      Delta = 0
      for x=1,4 do
         for y=1,4 do
            local v = 0
            for a=1,NA do
               local x2,y2 = trans(x,y, a)
               local R = reward(x,y, x2,y2, a) -- skipped x,y,a
               v = v + policy(x,y, a) * (R + gamma * V[x2][y2])
--               print(Delta,V[x][y],v,x,y, x2, y2, a)
            end
            Delta = math.max(Delta, math.abs(v - V[x][y]))
            V[x][y] = v

         end
      end
      steps = steps + 1
   until Delta < eps
   io.write(string.format("found in %d steps\n", steps))
   return V
end

function improve_policy(V)
   local P = torch.zeros(NA,NA)

   for x=1,4 do
      for y=1,4 do
         local max
         local max_i = 0
         for a=1,NA do
            local x2,y2 = trans(x,y, a)
            local R = reward(x,y, x2,y2, a)
            local v = R + gamma * V[x2][y2]

            if max_i == 0 or max < v then
               max_i = a
               max = v
            end
         end
         P[x][y] = max_i
      end
   end
   return P
end

function is_terminal(x,y)
   return x == 1 and y == 1 or x == N and y == N
end

function is_offboard(x,y)
   return x < 1 or x > N or y < 1 or y > N
end

function trans(x,y,a)
   local x2,y2
   if is_terminal(x,y) then x2,y2 = x,y
   elseif a == 1 then x2,y2 = x,y-1
   elseif a == 2 then x2,y2 = x-1,y
   elseif a == 3 then x2,y2 = x,y+1
   elseif a == 4 then x2,y2 = x+1,y
   else error "unknown action"
   end
   if is_offboard(x2,y2) then return x,y
   else return x2,y2
end
end

function reward(x,y, x2,y2, a)
   if is_terminal(x2,y2) then return 0
   elseif x == x2 and y == y2 then return -3
   else return -1
   end
end

function recursive_policy_improvement()
   local policy = function (x,y,a) return 0.25 end
   local old_P = torch.zeros(NA,NA)
   local vs
   local stale
   repeat
      vs = evaluate_states(trans, policy, reward)
      print(vs)
      local P = improve_policy(vs)
      stale = torch.all(torch.eq(old_P,P))
      old_P = P
      policy = function(x,y,a)
         if P[x][y] == a then return 1
         else return 0
         end
      end
      print(P)
   until stale

   return vs, P
end

recursive_policy_improvement()