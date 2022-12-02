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

signed main(){
  ios_base::sync_with_stdio(0);
  freopen("tmp.txt","r",stdin);
  string s;
  int ans=0,tmp=0;
  set<int> se;
  while(getline(cin,s)){
    if(s.empty()){
        ans=max(ans,tmp);
             se.insert(tmp);
             tmp=0;
    }else{
        tmp+=stoi(s);
    }
  }
  ans=max(ans,tmp);
  se.insert(tmp);
  auto it=se.rbegin();
  int a=0;
  a+=*it;
  ++it;
  a+=*it;
  ++it;
  a+=*it;
  //cout<<ans<<"\n" for part 1
  cout<<a<<"\n";
  return 0;
}
