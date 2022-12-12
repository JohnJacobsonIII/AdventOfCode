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

string s,screen[8];
int cur,x=1,ans,tmp;

void draw(){
  cout<<cur%40<<' '<<cur/40<<"\n";
  if(abs(x-cur%40)<=1){
    screen[cur/40].push_back('#');
  }else{
    screen[cur/40].push_back('.');
  }
}

void check(){
  if((cur-20)%40==0){
    ans+=cur*x;
  }
}

signed main(){
  ios_base::sync_with_stdio(0);
  freopen("tmp.txt","r",stdin);
  draw();
  while(cin>>s){
    if(s=="noop"){
        ++cur;
        check();
        draw();
    }else{
        ++cur;
        check();
        draw();
        ++cur;
        check();
        cin>>tmp;
        x+=tmp;
        draw();
    }
  }
  cout<<ans<<"\n";
  for(int i=0;i<8;++i){
    cout<<screen[i]<<"\n";
  }
  return 0;
}

