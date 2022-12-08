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

int getShapeScore(char c){
    if(c=='A'){
        return 1;
    }else if(c=='B'){
        return 2;
    }else if(c=='C'){
        return 3;
    }
    exit(-1);
}

int getResultScore(char c1,char c2){
    if(c1==c2){
        return 3;
    }else{
        if(c1=='A'&&c2=='B'||c1=='B'&&c2=='C'||c1=='C'&&c2=='A'){
            return 6;
        }
    }
    return 0;
}

void updateChar(char c1, char& c2){
    if(c2=='X'){
        if(c1=='A'){
            c2='C';
        }else if(c1=='B'){
            c2='A';
        }else{
            c2='B';
        }
    }else if(c2=='Y'){
        c2=c1;
    }else{
        if(c1=='A'){
            c2='B';
        }else if(c1=='B'){
            c2='C';
        }else{
            c2='A';
        }
    }
}

void update(char& c){
    if(c=='X'){
        c='A';
    }else if(c=='Y'){
        c='B';
    }else if(c=='Z'){
        c='C';
    }else{
        exit(-1);
    }
}

signed main(){
  ios_base::sync_with_stdio(0);
  freopen("tmp.txt","r",stdin);
  char c1,c2;
  int ans=0;
  while(cin>>c1){
    cin>>c2;
    //update(c2); this for part 1
    updateChar(c1,c2); //this for part 2
    ans+=(getShapeScore(c2)+getResultScore(c1,c2));
  }
  cout<<ans;
  return 0;
}
