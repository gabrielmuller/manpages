#include <iostream>
#include <cstring>
#include "avl_tree.h"
using namespace std;

int main (int argc, char** argv) {
  structures::AVLTree<int, int> tree ("test.dat", -1, -1);
  /*tree.insert(3, 3939);
  tree.insert(393, 393939);*/
  tree.test();
  return 0;
}
