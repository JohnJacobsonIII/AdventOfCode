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

vector<string> board;
string tmp;

//0 from left, 1 from right, 2 from top, 3 from bottom
char maxChar[110][110][4];
const int dx[]{0,0,-1,1},dy[]{-1,1,0,0};
int row,col;

bool visible(int i,int j){
    for(int dir=0;dir<4;++dir){
        if(board[i][j]>maxChar[i+dx[dir]][j+dy[dir]][dir]){
            return true;
        }
    }
    return false;
}

int getDirScore(int i,int j,int dir){
  int dist=1;
  for(;board[i][j]>board[i+dist*dx[dir]][j+dist*dy[dir]];++dist);
  return dist;
}

int getScore(int i,int j){
  int ans=1;
  for(int dir=0;dir<4;++dir){
    if(board[i][j]<=maxChar[i+dx[dir]][j+dy[dir]][dir]){
        ans*=getDirScore(i,j,dir);
    }else{
      if(dir==0){
        ans*=(j);
      }else if(dir==1){
        ans*=(col-j-1);
      }else if(dir==2){
        ans*=(i);
      }else{
        ans*=(row-i-1);
      }
    }
  }
}

signed main(){
  ios_base::sync_with_stdio(0);
  freopen("tmp.txt","r",stdin);
  while(cin>>tmp){
    board.push_back(tmp);
  }
  row=board.size(),col=board[0].size();
  for(int i=0;i<row;++i){
    maxChar[i][0][0]=board[i][0];
    maxChar[i][col-1][1]=board[i][col-1];
  }
  for(int j=0;j<col;++j){
    maxChar[0][j][2]=board[0][j];
    maxChar[row-1][j][3]=board[row-1][j];
  }
  for(int i=0;i<row;++i){
    for(int j=1;j<col;++j){
        maxChar[i][j][0]=max(board[i][j],maxChar[i][j-1][0]);
    }
  }
  for(int i=0;i<row;++i){
    for(int j=col-2;j>=0;--j){
        maxChar[i][j][1]=max(board[i][j],maxChar[i][j+1][1]);
    }
  }
  for(int i=1;i<row;++i){
    for(int j=0;j<col;++j){
        maxChar[i][j][2]=max(board[i][j],maxChar[i-1][j][2]);
    }
  }
  for(int i=row-2;i>=0;--i){
    for(int j=0;j<col;++j){
        maxChar[i][j][3]=max(board[i][j],maxChar[i+1][j][3]);
    }
  }
  int ans=0;
  int score=0;
  for(int i=1;i<row-1;++i){
    for(int j=1;j<col-1;++j){
        if(visible(i,j)){
            ++ans;
        }
        score=max(score,getScore(i,j));
    }
  }
  //this is for part 1
  cout<<(ans+2*col+2*row-4)<<"\n";
  //this is for part 2
  cout<<score<<"\n";
  return 0;
}

