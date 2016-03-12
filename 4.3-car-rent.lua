N = 20
MCAR=5
RentPrice = 10
MovePrice = 2

RentLambda1 = 3
RetLambda1 = 4

RentLambda2 = 3
RetLambda2 = 2

Gamma = 0.9
Eps = 0.01

function fac(n)
   local r = 1
   for i=2,n do
      r = r * i
   end
   return r
end

function poisson(lam, n)
   return math.exp(n * math.log(lam)) / fac(n) * math.exp(-lam)
end

function tabulate_poisson_diff(lam1, lam2)
   local tab = {}
   for t=-N,N do
      tab[t] = 0
      for a=0,N do
         local b = a - t
         if b >= 0 then
            tab[t] = tab[t] + poisson(lam1, a) * poisson(lam2, b)
         end
      end
   end
   return tab
end

function plot_table(tab)
   local min = 10e9
   local max = -10e9
   for i,v in pairs(tab) do
      if min > i then min = i end
      if max < i then max = i end
   end

   ten = torch.Tensor(max - min + 1)
   i=1
   for k=min,max do
      ten[i] = tab[k]
      i = i + 1
   end

   local gp = require 'gnuplot'
   gp.plot(ten)
end

PoissonDiff1 = tabulate_poisson_diff(RentLambda1, RetLambda1)
PoissonDiff2 = tabulate_poisson_diff(RentLambda2, RetLambda2)

-- REINFORCEMENT
function evaluate_policy(V, policy, trans, reward)
   local Delta
   local steps = 0
   repeat
      Delta = 0
      for i=0,N do
         for j=0,N do
            local v = 0
            for i2=0,N do
               for j2=0,N do
                  local a = policy(i,j) -- policy is deterministic
                  local pr = trans(i,j,  i2,j2, a) -- transition is probabilistic
                  local rw = reward(i,j, i2,j2, a)
                  v = v + pr * (rw + Gamma * V[i2][j2])
--                  print (i,j, i2, j2, pr, rw, v)
               end
            end
            Delta = math.max(Delta, math.abs(v - V[i][j]))
            V[i][j] = v
         end
      end
--      Renderer.updateV(V)
--      Renderer.draw()
      steps = steps + 1
      print(steps, Delta)

   until Delta < Eps
   io.write(string.format("found in %d steps\n", steps))
   return V
end

function policy0(i,j) -- do nothing
   return 0
end

function transfer(i,j,i2,j2, a)
   local di = i2 - i + a
   local dj = j2 - j - a

   return PoissonDiff1[di] * PoissonDiff2[dj]
end

function reward(i,j, i2, j2, a)
   local di = i2 - i + a
   local dj = j2 - j - a

   return -RentPrice * (math.min(di, 0) + math.min(dj, 0)) - MovePrice * math.abs(a)
end

function zeroV()
   local res = {}
   for i=0,N do
      res[i] = {}
      for j=0,N do
         res[i][j] = 0
      end
   end
   return res
end
