N = 10

class = require '30log'

-- Bandit
Bandit = class("Bandit")

function Bandit:init(n)
   self.size = n
   for i=1,n do
      self[i] = { mean = torch.normal(0,1), var = 1 }
   end
end

function Bandit:sample(k)
   return torch.normal(self[k].mean, self[k].var)
end

ValueLearner = class("ValueLearner")
function ValueLearner:init(n)
   self.N = n
   self.values = torch.zeros(n)
   self.weights = torch.zeros(n)
end

function ValueLearner:update(i, v)
   self.weights[i] = self.weights[i] + 1
   self.values[i] = self.values[i] + (v - self.values[i]) / self.weights[i]
end

function ValueLearner:zero()
   for i=1,N do
      self.values[i] = 0
      self.weights[i] = 0
   end
end

-- Greedy learner

EpsGreedyLearner = ValueLearner:extend("EpsGreedyLearner")

function EpsGreedyLearner:init(n, eps)
   EpsGreedyLearner.super.init(self, n)
   self.eps = eps
end

function EpsGreedyLearner:decide()
   local turn
   if torch.uniform() < self.eps then
      turn = torch.random(N)
   else
      _, turn = torch.max(self.values,1)
      turn = turn[1]
   end

   return turn
end

function EpsGreedyLearner:feedback(i, v)
   self:update(i, v)
end

SoftMax = class()
function SoftMax:init(n, t)
   self.N = n
   self.t = t
   self:zero()
end

function SoftMax:zero()
   self.exp_values = torch.ones(self.N) * 5
   self.sum = self.N * 5
   return self
end

function SoftMax:set(n, val)
   self.sum = self.sum - self.exp_values[n]
   self.exp_values[n] = math.exp(val / self.t)
   self.sum = self.sum + self.exp_values[n]
   return self
end

function SoftMax:sample()
   local r = torch.uniform(0, self.sum)
   local k = 1
   while k <= self.N do
      r = r - self.exp_values[k]
      if r <= 0 then
         return k
      end
      k = k + 1
   end
end

SMLearner = ValueLearner:extend("SMLearner")
function SMLearner:init(n,t)
   SMLearner.super.init(self, n)
   self.sm = SoftMax(n,t)
end

function SMLearner:zero()
   self.sm:zero()
   SMLearner.super.zero(self)
end

function SMLearner:decide()
   local turn = self.sm:sample()
   return turn
end

function SMLearner:feedback(i, v)
   self:update(i,v)
   self.sm:set(i, self.values[i])
end

function experiment(bandit, learner, n)
   local hist = torch.Tensor(n)

   for i=1,n do
      local turn = learner:decide()
      local res = bandit:sample(turn)
      learner:feedback(turn, res)
      hist[i] = res
   end

   return hist
end

function average_experiments(bandit, learner, exp_len, exp_num)
   local res = torch.Tensor(exp_num, exp_len)
   for i=1,exp_num do
      learner:zero()
      res[i] = experiment(bandit, learner, exp_len)
   end
   return res:mean(1):t()
end

N = 10
b = Bandit(N)
avg = 200

learner1 = EpsGreedyLearner(10, 0.01)
learner2 = EpsGreedyLearner(10, 0.1)
learner3 = SMLearner(10, 0.1)

res1 = average_experiments(b, learner1, 1000, avg)
res2 = average_experiments(b, learner2, 1000, avg)
res3 = average_experiments(b, learner3, 1000, avg)

print(b)
--print(egl.num_turns)
--print(egl.values)

gp = require 'gnuplot'

gp.plot({"greedy 99%", res1, "-"}, {"greedy 90%", res2, "-"}, {"SoftMax", res3, "-"})

print(res1:sum(), res2:sum(), res3:sum())
