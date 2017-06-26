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

template <typename T, typename S>
class AVLTree {
private:
 struct Node;
 std::size_t size_{0}; /**< Tamanho da árvore. */

 public:
   //terrível
   struct tree_info {
     std::string filename;  /**< FileIO */
     T t_null; /**valor null de T*/
     S s_null;
     size_t* total_size;
   };
   tree_info info;

   AVLTree(std::string filename_, T t_null_, S s_null_) {
     info.filename = filename_;
     info.t_null = t_null_;
     info.s_null = s_null_;
     info.total_size = &size_;
     root(new Node(info));
   }

  /** Insere dado.
   *  \param data dado a inserir.
   */
  Node* root() {
    return new Node(0, info);
  }

  void root(Node* node) {
    node->writeMe(0, info);
  }

  void insert(const T& data, const S& second) {
    if (empty()) {
      root(new Node(data, second, info));
    } else {
      root()->insert(data, second);
    }
    size_++;
  }

  /** enjambreitioon
   *
   */
  S find(T data) {
    Node* it = root();

    while (!it->isNull()) {
      T itd = it->data;

      if (itd == data) {
        return it->second;
      } else if (itd > data) {
        it = it->left();
      } else if (itd < data) {
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
    int index;
    tree_info info;
    T data;             /**< Dado. */
    S second;           /** Dado mapeado */

    Node(tree_info info_) {
      reset(info_);
    }
    Node(const T data_, const S second_, tree_info info_) {
      data = data_;
      second = second_;
      info = info_;
    }
    void reset(tree_info info_) {
      info = info_;
      data = info.t_null;
      second = info.s_null;
      index = 0;
    }
    bool isNull() {
      return data == info.t_null;
    }

    Node(int index, tree_info info_) {
      info = info_;
      /*if (index < 0) {
        std::cout << index << " eita nois \n" << std::flush;
        throw std::out_of_range("Index negativo");
      }*/
      if (index < 0 || index >= *(info.total_size)) {
        reset(info);
      } else {
        std::ifstream i(info.filename);
        int tsize = sizeof(T);
        int ssize = sizeof(S);
        int start = index*(tsize+ssize);
        i.seekg(start);
        std::cout << index << " " << i.good() << std::endl << std::flush;

        i.read((char*) &data, tsize);
        i.seekg(start + tsize);
        i.read((char*) &second, ssize);
      }
    }

    void writeMe(int index, tree_info info_) {
      info = info_;
      std::ofstream o(info.filename);
      int tsize = sizeof(T);
      int ssize = sizeof(S);
      int start = index*(tsize+ssize);
      o.seekp(start);
      o.write((char*) &data, tsize);
      o.seekp(start + tsize);
      o.write((char*) &second, ssize);
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
      node->writeMe(index*2, info);
    }

    void right(Node* node) {
      node->writeMe(index*2 +1, info);
    }

    void parent(Node* node) {
      node->writeMe(index / 2, info);
    }

    std::size_t height; /**< Altura do nodo. */

    /** Insere dado.
     *  \param data dado a inserir.
     */
    void insert(const T& data_, const S& second_) {
      Node* r = right();
      Node* l = left();

      if (data_ > data) {
        if (r->isNull()) {
          right(new Node(data_, second_, info));
          //right()->parent(this);
          right()->rebalance();
        } else {
          r->insert(data_, second_);
        }
      } else {
        if (left()->isNull()) {
          left(new Node(data_, second_, info));
          //left->parent = this;
          left()->rebalance();
        } else {
          l->insert(data_, second_);
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
