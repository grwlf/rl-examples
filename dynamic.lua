N = 4
NA = 4
eps = 0.001
gamma = 0.9

function evaluate_policy(V, trans, policy, reward)
   local Delta
   local steps = 0
   repeat
      Delta = 0
      for i=1,4 do
         for j=1,4 do
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
      steps = steps + 1
   until Delta < eps
   io.write(string.format("found in %d steps\n", steps))
   return V
end

function improve_policy(V)
   local P = torch.zeros(NA,NA)

   for i=1,4 do
      for j=1,4 do
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

function reward(i,j, i2,j2, a)
   if is_terminal(i2,j2) then return 0
   elseif i == i2 and j == j2 then return -3
   else return -1
   end
end

function recursive_policy_improvement()
   local V = torch.zeros(N,N)
   local old_P = torch.ones(NA,NA)
   local P = old_P
   local stale
   repeat
      local policy = function(i,j,a)
         if P[i][j] == a then return 1
         else return 0
         end
      end

      V = evaluate_policy(V, trans, policy, reward)
      print(V)
      P = improve_policy(V)
      stale = torch.all(torch.eq(old_P,P))
      old_P = P
      print(P)
   until stale

   return vs, P
end

recursive_policy_improvement()
