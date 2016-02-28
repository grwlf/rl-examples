N = 10

-- Bandit
Bandit = {}
Bandit.__index = Bandit

function Bandit.new(n)
   local o = {}
   setmetatable(o, Bandit)

   o.size = n
   for i=1,n do
      o[i] = { mean = torch.uniform(10,50), var = 1 }
   end
   return o
end

function Bandit:sample(k)
   return torch.normal(self[k].mean, self[k].var)
end

-- Greedy learner

EpsGreedyLearner = {}
EpsGreedyLearner.__index = EpsGreedyLearner

function EpsGreedyLearner.new(b, eps)
   local o = {}
   setmetatable(o, EpsGreedyLearner)

   o.bandit = b
   o.eps = eps
   o.values = torch.zeros(b.size)
   o.num_turns = torch.zeros(b.size)

   return o
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


b = Bandit.new(10)
egl = EpsGreedyLearner.new(b, 0.01)
res = egl:experiment(2000)

print(b)
print(egl.num_turns)

gp = require 'gnuplot'

gp.plot(res)
