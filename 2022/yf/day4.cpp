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

using namespace std;
#define int long long

pair<int,int> pa,pb;
char c;
bool isIn(pair<int,int>& pa, pair<int,int>& pb){
    return pa.first<=pb.first&&pa.second>=pb.second;
}

bool overlap(pair<int,int>& pa, pair<int,int>& pb){
    return pa.first<=pb.first&&pa.second>=pb.first&&pa.second<=pb.second;
}

signed main(){
  ios_base::sync_with_stdio(0);
  freopen("tmp.txt","r",stdin);
  int ans=0;
  while(cin>>pa.first){
    cin>>c>>pa.second>>c;
    cin>>pb.first>>c>>pb.second;
    if(isIn(pa,pb)||isIn(pb,pa)){
    //    ++ans;;
    }
    if(isIn(pa,pb)||isIn(pb,pa)||overlap(pa,pb)||overlap(pb,pa)){
        ++ans;
    }
  }
  cout<<ans<<"\n";
  return 0;
}

