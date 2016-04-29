#include <iostream>
#include <vector>
#include <list>
#include <map>
#include <random>
#include <cstdlib>

using namespace std;

enum class Action { L,R,U,D };

typedef vector<Action> Actions;

struct State {
  int x;
  int y;
};

bool operator<(const State &s1, const State &s2) {
  if(s1.x != s2.x) return s1.x < s2.x;
  if(s1.y != s2.y) return s1.y < s2.y;
  return false;
}


struct GW {
  int sx;
  int sy;
};

bool mc_is_terminal(GW gw, State s) {
  return (s.x ==0 && s.y==0 ) || (s.x == gw.sx-1 && s.y==gw.sy-1);
}

// [0..max]
/* int myrand(int max) { */
/*   return rand()%(max+1); */
/* } */

static random_device rd;
static mt19937_64 mt(rd());

/* // [0..max] */
int myrand(int max) {
  uniform_int_distribution<int> dist(0, max);
  return dist(mt);
}


State mc_state (GW gw) {
  return State {myrand(gw.sx-1), myrand(gw.sy-1)};
}

Actions mc_actions(GW gw, State s) {
  if(mc_is_terminal(gw,s)) {
    return Actions();
  }
  else {
    Actions a = {Action::L,Action::R,Action::U,Action::D};
    return a;
  }
}

State check(GW gw, State s, State def) {
  if ( s.x >= 0 && s.x < gw.sx && s.y >= 0 && s.y < gw.sy )
    return s;
  else
    return def;
}

State mc_transition(GW gw, State s, Action a) {
  switch(a) {
    case Action::L: return check(gw, State{s.x-1, s.y}, s); break;
    case Action::R: return check(gw, State{s.x+1, s.y}, s); break;
    case Action::U: return check(gw, State{s.x, s.y-1}, s); break;
    case Action::D: return check(gw, State{s.x, s.y+1}, s); break;
  };
}

int mc_reward(GW gw, State s, Action a, State s2) {
  return -1;
}

struct Move {
  State s;
  Action a;
  State s2;
};

bool operator<(const Move &m1, const Move &m2) {
  if(m1.s < m2.s || m2.s < m1.s) return m1.s < m2.s;
  if(m1.a < m2.a || m1.a < m2.a) return m1.a < m2.a;
  if(m1.s2 < m2.s2 || m2.s2 < m1.s2) return m2.s2 < m2.s2;
  return false;
}

typedef list<Move> Episode;

Episode episode(GW gw, State s) {
  Episode e;
  int n = 0;

  while(1) {
    if(mc_is_terminal(gw, s))
      break;
    Actions as = mc_actions(gw,s);

    int idx = myrand(as.size()-1);
    Action a = as[idx];

    State s2 = mc_transition(gw,s,a);

    e.push_back(Move { s, a, s2 });
    s = s2;
    n++;
  }

  return e;
}

map<State,int> backtrack_fv(GW gw, Episode e) {
  map<State,int> g;

  int r = 0;
  for (auto m = e.rbegin(); m != e.rend(); ++m) {
    r += mc_reward(gw, m->s, m->a, m->s2);
    g[m->s] = r;
  }

  return g;
}

struct CRA {
  double c;
  size_t n;
};

CRA meld(CRA cra, double v) {
  return CRA { cra.c + (v - cra.c)/(cra.n+1), cra.n+1};
}

map<State, double> answer = {
  {{0,0},0.0}, {{0,1},-14.0}, {{0,2},-20.0}, {{0,3},-22.0},
  {{1,0},-14.0}, {{1,1},-18.0}, {{1,2},-20.0}, {{1,3},-20.0},
  {{2,0},-20.0}, {{2,1},-20.0}, {{2,2},-18.0}, {{2,3},-14.0},
  {{3,0},-22.0}, {{3,1},-20.0}, {{3,2},-14.0}, {{3,3},0.0}
};

map<State,CRA> eval (GW gw, int count){
  map<State,CRA> v;
  for(int i=0; i<count; i++) {

    /* if(i==290000) { */
    /*   cout << i << endl; */
    /* } */

    State s = mc_state(gw);
    /* State s = {0,3}; */
    Episode e = episode(gw,s);
    map<State,int> g = backtrack_fv(gw, e);
    if(v.find(s) == v.end()) {
      v[s] = CRA {0,0};
    }
    v[s] = meld (v[s], g[s]);
    /* for(auto s : g) { */
    /*   if(v.find(s.first) == v.end()) { */
    /*     v[s.first] = CRA {0,0}; */
    /*   } */
    /*   v[s.first] = meld(v[s.first], s.second); */
    /* } */

    /* double err = answer[s] - v[s].c; */

    double err = 0;
    for(auto s:v) {
      err += answer[s.first] - s.second.c;
    }
    cout << i << "\t" << err << endl;
  }

  return v;
}

void showv(GW gw, map<State,CRA> v) {
  for(int y=0; y<gw.sy; y++) {
    for(int x=0; x<gw.sx; x++) {
      auto i = v.find(State{x,y});
      if(i == v.end())
        cerr << "         " ;
      else
        cerr << i->second.c << "( " << i->second.n << ")" ;
    }
    cerr << endl;
  }
}

int main() {

  mt.seed(56);

  GW gw = {4,4};
  map<State,CRA> v = eval(gw, 900000);

  showv(gw, v);
  return 0;
}


