#include <iostream>
#include <map>
#include <string>
using namespace std;

int main () {
  map<int, string>  test_tree= {{1, "abc"}, {2, "BBBBBB"}, {3, "tois"}};
  map<int, string>::iterator it = test_tree.find(2);
  if (it != test_tree.end()) {
    cout <<  it->second << endl;
  }
  return 0;
}
