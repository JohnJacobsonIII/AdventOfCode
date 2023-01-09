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

const int dx[]{1,-1,0,0},dy[]{0,0,1,-1},SZ_PART1=2,SZ_PART2=10;
const bool part2=true;

int charArr[128],ans,dir,mv;
char c;
set<pa> m;

struct Snake{
  int sz;
  pa S[part2?SZ_PART2:SZ_PART1];
  void move(int dir){
    S[0].first+=dx[dir];
    S[0].second+=dy[dir];
    for(int i=1;i<sz;++i){
        if(abs(S[i].first-S[i-1].first)>=2||abs(S[i].second-S[i-1].second)>=2){
            int dxx=S[i-1].first-S[i].first,dyy=S[i-1].second-S[i].second;
            if(abs(dxx)==2){
                dxx/=2;
            }
            if(abs(dyy)==2){
                dyy/=2;
            }
            S[i].first+=dxx;
            S[i].second+=dyy;
        }
        if(i==sz-1){
            if(m.find(S[i])==m.end()){
                ++ans;
                m.insert(S[i]);
            }
        }
    }
  }
  explicit Snake(){
    sz=part2?SZ_PART2:SZ_PART1;
    for(int i=0;i<sz;++i){
        S[i]=make_pair(0,0);
    }
    m.insert(S[sz-1]);
  }
};

signed main(){
  ios_base::sync_with_stdio(0);
  freopen("tmp.txt","r",stdin);
  charArr['T']=0,charArr['D']=1;
  charArr['R']=2,charArr['L']=3;
  Snake snake;
  while(cin>>c>>mv){
    dir=charArr[c];
    while(mv--){
        snake.move(dir);
    }
  }
  cout<<m.size()<<endl;
  return 0;
}

