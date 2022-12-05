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

int ch[128],rec[128];

void handlePart1(int &ans){
  string s;
  while(cin>>s){
    for(int i=0;i<s.size()/2;++i){
        rec[s[i]]++;
    }
    for(int i=s.size()/2;i<s.size();++i){
        if(rec[s[i]]>0){
            rec[s[i]]=-1;
            ans+=ch[s[i]];
        }
    }
    memset(rec,0,sizeof rec);
  }
}

void handlePart2(int& ans){
  vector<string> vec;
  string s;
  while(cin>>s){
    sort(s.begin(),s.end());
    s.erase(unique(s.begin(),s.end()),s.end());
    vec.push_back(s);
    for(int i=0;i<2;++i){
        cin>>s;
        sort(s.begin(),s.end());
        s.erase(unique(s.begin(),s.end()),s.end());
        vec.push_back(s);
    }
    for(int i=0;i<vec.size();++i){
        for(char c:vec[i]){
            rec[c]++;
        }
    }
    for(int i=0;i<128;++i){
        if(rec[i]==vec.size()){
            ans+=ch[i];
        }
    }
    memset(rec,0,sizeof rec);
    vec.clear();
  }
}


signed main(){
  ios_base::sync_with_stdio(0);
  freopen("tmp.txt","r",stdin);
  for(int i='a';i<='z';++i){
    ch[i]=i-'a'+1;
  }
  for(int i='A';i<='Z';++i){
    ch[i]=i-'A'+27;
  }
  int ans=0;
  //handlePart1(ans);
  handlePart2(ans);
  cout<<ans;
  return 0;
}
