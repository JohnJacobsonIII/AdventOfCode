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


struct Dir{
  string name;
  int parent,size;
  vector<int> items;
  explicit Dir(string name="",int size=0,int parent=-1):name(name),size(size),parent(parent){};
  void addFile(vector<Dir>& dirs, int item){
    items.push_back(item);
    updateSize(dirs,dirs[item].size);
  }
  void updateSize(vector<Dir>& dirs, int size){
    this->size+=size;
    if(parent!=-1){
        dirs[parent].updateSize(dirs,size);
    }
  }
  bool isDir(){return !items.empty();}
};

//normal file as a dir with 0 items;
vector<Dir> dirs{Dir("/")};
int curDir;
string tmp,curName="/",pa1,pa2,dirName;
unordered_set<string> dirNames{"/"};

void handleInput(){
  while(getline(cin,tmp)){
    if(tmp[0]=='$'){
        if(auto pos=tmp.find("cd");pos!=string::npos){
            tmp=tmp.substr(pos+3);
            if(tmp=="/"){
                curDir=0,curName="/";
            }else if(tmp==".."){
                curDir=dirs[curDir].parent;
                curName=dirs[curDir].name;
            }else{
                tmp=curName+tmp+"/";
                bool found=false;
                for(size_t i=0;i<dirs[curDir].items.size()&&!found;++i){
                    if(dirs[dirs[curDir].items[i]].name==tmp){
                        curDir=dirs[curDir].items[i];
                        curName=tmp;
                        found=true;
                    }
                }
                if(!found){
                    exit(-1);
                }
            }
        }
    }else{
        auto pos=tmp.find(" ");
        pa1=tmp.substr(0,pos);
        pa2=tmp.substr(pos+1);
        dirName=curName+pa2+"/";
        if(dirNames.find(dirName)!=dirNames.end()){
            continue;
        }
        dirNames.insert(dirName);
        if(pa1=="dir"){
            dirs.push_back(Dir(dirName,0,curDir));
            dirs[curDir].items.push_back(dirs.size()-1);
        }else{
            dirs.push_back(Dir(dirName,stoi(pa1),curDir));
            dirs[curDir].addFile(dirs,dirs.size()-1);
        }
    }
  }
}

const int MAX_SIZE=100000;
const int MAX_SIZE2=30000000;

signed main(){
  ios_base::sync_with_stdio(0);
  freopen("tmp.txt","r",stdin);
  handleInput();
  int ans=0,ans2=0x3f3f3f3f,left=70000000-dirs[0].size;
  for(size_t i=0;i<dirs.size();++i){
    if(dirs[i].isDir()&&dirs[i].size<=MAX_SIZE){
        ans+=dirs[i].size;
    }
    if(dirs[i].isDir()&&dirs[i].size+left>=MAX_SIZE2){
        ans2=min(ans2,dirs[i].size);
    }
  }
  cout<<ans<<"\n";
  cout<<ans2<<"\n";
  return 0;
}

