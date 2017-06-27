/* Copyright [2017] <Gabriel Müller>
 * array_list.h
 */

#ifndef STRUCTURES_AVL_TREE_H_
#define STRUCTURES_AVL_TREE_H_

#include <stdexcept>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cstring>
namespace structures {
/** Classe árvore AVL.
 *
 */

template <typename F, typename S>
class AVLTree {
private:
 struct Node;
 std::size_t size_{0}; /**< Tamanho da árvore. */

 public:
   //terrível
   struct tree_info {
     std::string filename;  /**< FileIO */
     F t_null; /**valor null de F*/
     S s_null;
     size_t* total_size;
     std::fstream* io;
   };
   tree_info info;

   ~AVLTree() {
   	(info.io)->close();

   	std::cout << "so long..." << std::endl;
   }
   AVLTree(std::string filename_, F t_null_, S s_null_) {
     info.filename = filename_;
     info.t_null = t_null_;
     info.s_null = s_null_;
     info.total_size = &size_;
     info.io = new std::fstream(info.filename, std::ios::binary | std::ios::app | std::ios::out);

     root(new Node(info));
   }
	void printfile() {
		for (int i = 0; i < 100; i++) {
			(info.io)->seekg(i*sizeof(int));
			int r;
			(info.io)->read((char*) &r, sizeof(int));
			std::cout << r << ", ";
		}
		std::cout << std::endl;
	}
  void test() {
	size_ = 1000;
    root()->print();
    root()->left(new Node(5, 77, 2, info));
    root()->left()->print();
    //root()->insert(5, 7);
    this->printfile();
  }


  Node* root() {
    return new Node(1, info);
  }

  void root(Node* node) {
    node->writeMe(1);
    std::cout << "yep ";
    node->print();
  }

  void insert(const F& first, const S& second) {
    if (empty()) {
      root(new Node(first, second, 1, info));
    } else {
      root()->insert(first, second);
    }
    size_++;
  }

  /** enjambreitioon
   *
   */
  S find(F first) {
    Node* it = root();

    while (!it->isNull()) {
      F itd = it->first;

      if (itd == first) {
        return it->second;
      } else if (itd > first) {
        it = it->left();
      } else if (itd < first) {
        it = it->right();
      }
    }

    //não achou
    return info.s_null;
  }

  /** Árvore vazia?
   *  \return true se vazia.
   */
  bool empty() { return root()->isNull(); }

 private:
  /** Struct Node.
   *  Nodo da árvore.
   *  Pode ser interpretado como uma árvore ou sub-árvore.
   */
  struct Node {
    int index{-1};
    tree_info info;
    F first;             /**< Dado. */
    S second;           /** Dado mapeado */

    Node(tree_info info_) {
      reset(info_);
    }
    Node(const F first_, const S second_, int index_, tree_info info_) {
      first = first_;
      second = second_;
      info = info_;
      index = index_;
    }
    void reset(tree_info info_) {
      info = info_;
      first = info.t_null;
      second = info.s_null;
      index = 1;
    }
    bool isNull() {
      return first == info.t_null;
    }

    Node(int index_, tree_info info_) {
      index = index_;
      info = info_;
      /*if (index < 0) {
        std::cout << index << " eita nois \n" << std::flush;
        throw std::out_of_range("Index negativo");
      }*/
      if (index < 1 || index > *(info.total_size)) {
      	std::cout << "index " << index << " is wrong \n";
        reset(info);
      } else {
		if (!(info.io)->is_open()) {
      		std::cerr << "arquivo não pôde ser aberto para leitura." << std::endl;
		}
        int tsize = sizeof(F);
        int ssize = sizeof(S);
        int start = index*(tsize+ssize);
        (info.io)->seekg(start);
        (info.io)->read((char*) &first, tsize);
        (info.io)->seekg(start + tsize);
        (info.io)->read((char*) &second, ssize);
      }
    }

    void print() {
      std::cout << "(" << first << ", " << second << ")" << std::endl;
    }
    void writeMe(int index_) {
      index = index_;
      if (!(info.io)->is_open()) {
      	std::cerr << "arquivo não pôde ser aberto para escrita." << std::endl;
      }
      int tsize = sizeof(F);
      int ssize = sizeof(S);
      int start = index*(tsize+ssize);
      std::cout << "index " << index << " start " << start
      << " first " << first << " second " << second << "\n";
      (info.io)->seekp(start);
      (info.io)->write((char*) &first, tsize);
      (info.io)->seekp(start + tsize);
      (info.io)->write((char*) &second, ssize);
      (info.io)->write("Hello", 50);
    }
	

    Node* left() {
      return new Node(index*2, info);
    }

    Node* right() {
      return new Node(index*2 +1, info);
    }

    Node* parent() {
      if (index == 0) {
        return new Node(info);
      } else {
        return new Node(index/2, info);
      }
    }

    void left(Node* node) {
      node->writeMe(index*2);
    }

    void right(Node* node) {
      node->writeMe(index*2 +1);
    }

    void parent(Node* node) {
      node->writeMe(index / 2);
    }

    std::size_t height; /**< Altura do nodo. */

    /** Insere dado.
     *  \param first dado a inserir.
     */
    void insert(const F& first_, const S& second_) {
      Node* r = right();
      Node* l = left();

      if (first_ > first) {
        if (r->isNull()) {
          std::cout << "right null\n";
          right(new Node(first_, second_, index*2 +1, info));
          //right()->parent(this);
          right()->rebalance();
        } else {
          r->insert(first_, second_);
        }
      } else {
        if (left()->isNull()) {
          std::cout << "left null\n";
          left(new Node(first_, second_, index*2, info));
          //left->parent = this;
          left()->rebalance();
        } else {
          l->insert(first_, second_);
        }
      }
    }

    void rebalance() {
      Node* r = parent();
      while (!(r->parent()->isNull())) {
        r = r->parent();
      }
      r->updateHeight();
      for (Node* i = parent(); !i->isNull(); i = i->parent()) {
        if (i->factor() == 2) {
          if (i->right()->factor() == 1) {
            simpleLeft();
          } else if (i->right()->factor() == -1) {
            doubleLeft();
          }
        } else if (i->factor() == -2) {
          if (i->left()->factor() == -1) {
            simpleRight();
          } else if (i->right()->factor() == 1) {
            doubleRight();
          }
        }
      }
    }

    int factor() {
      int l, r;
      l = !(left()->isNull()) ? left()->height : -1;
      r = !(right()->isNull()) ? right()->height : -1;
      return r - l;
    }

    void updateHeight() {
      height = 0;
      Node* l = left();
      Node* r = right();

      if (!l->isNull()) {
        l->updateHeight();
        height += l->height;
      }
      if (!r->isNull()) {
        r->updateHeight();
        height += r->height;
      }
    }

    void simpleLeft() {
      Node* R = right();
      Node* RL = R->left();
      right(RL);
      R->left(this);
      parent(R);
    }

    void simpleRight() {
      Node* L = left();
      Node* LR = L->right();
      left(LR);
      L->right(this);
      parent(L);
    }

    void doubleLeft() {
      Node* R = right();
      Node* RL = R->left();
      Node* RLR = RL->right();
      R->left(RLR);
      RL->right(R);
      Node* RLL = RL->left();
      right(RLL);
      RL->left(this);
      parent(RL);
    }

    void doubleRight() {
      Node* L = left();
      Node* LR = L->right();
      Node* LRL = LR->left();
      L->right(LRL);
      LR->left(L);
      Node* LRR = LR->right();
      left(LRR);
      LR->right(this);
      parent(LR);
    }

  };
};
}  // namespace structures

#endif
