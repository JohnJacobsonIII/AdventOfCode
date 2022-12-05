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

string str[9]{"MSJLVFNR","HWJFZDNP","GDCRW","SBN","NFBCPWZM","WMRP","WSLGNTR","VBNFHTQ","FNZHML"};
//string str[3]{"NZ","DCM","P"};
string tmp;
int n,from,to;

void handleInst(){
    while(cin>>tmp){
        cin>>n>>tmp>>from>>tmp>>to;
        --from;--to;
        tmp=string(str[from].end()-n,str[from].end());
        //reverse(tmp.begin(),tmp.end()); uncomment this for part 1
        str[to]+=tmp;
        while(n--){
            str[from].pop_back();
        }
        //cout<<str[from]<<" "<<str[to]<<"\n";
    }
}


signed main(){
  ios_base::sync_with_stdio(0);
  freopen("tmp.txt","r",stdin);
  for(int i=0;i<9;++i){
    reverse(str[i].begin(),str[i].end());
  }
  handleInst();
  for(int i=0;i<9;++i){
    cout<<str[i].back();
  }
  return 0;
}

