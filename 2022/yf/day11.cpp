#include <iostream>
#include <cstring>
#include <algorithm>
#include <deque>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <cctype>
#include <numeric>
#include <functional>
#include <queue>

using namespace std;
using pa=pair<int,int>;
#define int long long
#define oper(t) [](int x){return x t ;}

struct Monkey{
  int test;
  function<int(int)> op;
  int br[2];
  deque<int> items;
  int cnt;
}monkeys[8];

const int module=17*7*13*2*19*3*5*11;
const bool part2=true;

void inspect(int i){
    while(!monkeys[i].items.empty()){
        ++monkeys[i].cnt;
        auto tmp=monkeys[i].items[0];
        monkeys[i].items.pop_front();
        tmp=monkeys[i].op(tmp);
        if(part2){
          tmp%=module;
        }else{
          tmp/=3;
        }
        monkeys[monkeys[i].br[tmp%monkeys[i].test!=0]].items.push_back(tmp);
    }
}

signed main(){
  ios_base::sync_with_stdio(0);
  cout<<module<<' '<<module*module<<"\n";
  freopen("tmp.txt","r",stdin);
  monkeys[0]={17,oper(*5),{4,7},{89,74}};
  monkeys[1]={7,oper(+3),{3,2},{75, 69, 87, 57, 84, 90, 66, 50}};
  monkeys[2]={13,oper(+7),{0,7},{55}};
  monkeys[3]={2,oper(+5),{0,2},{69, 82, 69, 56, 68}};
  monkeys[4]={19,oper(+2),{6,5},{72, 97, 50}};
  monkeys[5]={3,oper(*19),{6,1},{90, 84, 56, 92, 91, 91}};
  monkeys[6]={5,oper(*x),{3,1},{63, 93, 55, 53}};
  monkeys[7]={11,oper(+4),{5,4},{ 50, 61, 52, 58, 86, 68, 97}};
  for(int i=0;i<(part2?10000:20);++i){
    for(int i=0;i<8;++i){
        inspect(i);
    }
  }
  sort(monkeys,monkeys+8,[](Monkey& a, Monkey& b){return a.cnt>b.cnt;});
  cout<<monkeys[0].cnt*monkeys[1].cnt<<endl;
  return 0;
}

