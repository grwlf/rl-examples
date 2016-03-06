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

-- Greedy learner

EpsGreedyLearner = class("EpsGreedyLearner")

function EpsGreedyLearner:init(b, eps)
   self.bandit = b
   self.eps = eps
   self.values = torch.zeros(b.size)
   self.num_turns = torch.zeros(b.size)
end

function EpsGreedyLearner:experiment(n)
   local hist = torch.Tensor(n)

   local turn
   for i=1,n do
      if torch.uniform() < self.eps then
         turn = torch.random(self.bandit.size)
      else
         _, turn = torch.max(self.values,1)
         turn = turn[1]
      end

      local res = self.bandit:sample(turn)
      hist[i] = res

      self.values[turn] = (self.values[turn] * self.num_turns[turn] + res) / (self.num_turns[turn] + 1)
      self.num_turns[turn] = self.num_turns[turn] + 1
   end

   return hist
end

function EpsGreedyLearner:zero()
   for i=1,self.bandit.size do
      self.values[i] = 0
      self.num_turns[i] = 0
   end
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

SMLearner = class()
function SMLearner:init(b,t)
   self.sm = SoftMax(b.size,t)
   self.bandit = b
   self.values = torch.zeros(b.size)
   self.num_turns = torch.zeros(b.size)
end

function SMLearner:zero()
   self.sm:zero()
end

function SMLearner:experiment(n)
   local hist = torch.Tensor(n)

   local turn
   for i=1,n do
      turn = self.sm:sample()

      local res = self.bandit:sample(turn)
      hist[i] = res

      self.values[turn] = (self.values[turn] * self.num_turns[turn] + res) / (self.num_turns[turn] + 1)
      self.num_turns[turn] = self.num_turns[turn] + 1
      self.sm:set(turn, self.values[turn])
   end

   return hist
end


function average_experiments(learner, exp_len, exp_num)
   local res = torch.Tensor(exp_num, exp_len)
   for i=1,exp_num do
      learner:zero()
      res[i] = learner:experiment(exp_len)
   end
--   print(res)
   return res:mean(1):t()
end

b = Bandit(10)
avg = 200

learner1 = EpsGreedyLearner(b, 0.01)
learner2 = EpsGreedyLearner(b, 0.1)
learner3 = SMLearner(b,0.1)

res1 = average_experiments(learner1, 1000, avg)
res2 = average_experiments(learner2, 1000, avg)
res3 = average_experiments(learner3, 1000, avg)

print(b)
--print(egl.num_turns)
--print(egl.values)

gp = require 'gnuplot'

gp.plot({"greedy 99%", res1, "-"}, {"greedy 90%", res2, "-"}, {"SoftMax", res3, "-"})

print(res1:sum(), res2:sum(), res3:sum())
