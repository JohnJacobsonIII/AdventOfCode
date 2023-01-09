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

const bool part2=true;

queue<pa> q;
string tmp;
deque<string> board;
vector<vector<int>> vec;
const int dx[]{1,-1,0,0},dy[]{0,0,1,-1};
bool check(int i,int j, int dir){
  if(part2){
    return i+dx[dir]>=1&&j+dy[dir]>=1&&
            i+dx[dir]<board.size()-1&&
            j+dy[dir]<board[0].size()-1&&
            board[i][j]<=board[i+dx[dir]][j+dy[dir]]+1;
  }
  return board[i][j]+1>=board[i+dx[dir]][j+dy[dir]];
}
pa S,E;



signed main(){
  ios_base::sync_with_stdio(0);
  freopen("tmp.txt","r",stdin);
  for(int cur=0;cin>>tmp;++cur){
    board.push_back((char)('z'+3)+tmp+(char)('z'+3));
    for(int i=0;i<board.back().size();++i){
      if(board[cur][i]=='S'){
        S={cur+1,i};
        board[cur][i]='a';
      }else if(board[cur][i]=='E'){
        E={cur+1,i};
        board[cur][i]='z';
      }
    }
  }

  if(part2){
    swap(S,E);
  }

  board.push_back(string(board.back().size(),'z'+3));
  board.push_front(board.back());

  vec.resize(board.size());
  for(int i=0;i<board.size();++i){
    vec[i].resize(board[i].size());
    fill(vec[i].begin(),vec[i].end(),-1);
  }
  q.push(S);
  vec[S.first][S.second]=0;
  while(!q.empty()){
    auto t=q.front();
    q.pop();
    if(part2&&board[t.first][t.second]=='a'){
        cout<<vec[t.first][t.second];
        return 0;
    }
    if(t==E&&!part2){
        break;
    }
    int i=t.first,j=t.second;
    for(int dir=0;dir<4;++dir){
        if(vec[i+dx[dir]][j+dy[dir]]==-1&&check(i,j,dir)){
            vec[i+dx[dir]][j+dy[dir]]=vec[i][j]+1;
            q.push(make_pair(i+dx[dir],j+dy[dir]));
        }
    }
  }
  if(!part2){
    cout<<vec[E.first][E.second];
  }
  return 0;
}

