/* Copyright [2017] <Gabriel Müller>
 * array_list.h
 */

#ifndef STRUCTURES_AVL_TREE_H_
#define STRUCTURES_AVL_TREE_H_

#include <stdexcept>
#include <cstdlib>
#include "array_list.h"

namespace structures {

/** Classe árvore AVL.
 *
 */
template <typename T, typename S>
class AVLTree {
 public:

   AVLTree(std::string filename, T t_null_, S s_null_) {
     stream (filename, ios::trunc);
     t_null = t_null_;
     s_null = s_null_;
   }
  /** Destrutor.
   *

  ~AVLTree() {
    if (root != nullptr) {
      root->delete_all();
      root = nullptr;
    }
    size_ = 0;
  }

  /** Insere dado.
   *  \param data dado a inserir.
   */
  Node* root() {
    return new Node(0);
  }

  void root(Node* node) {
    node->writeMe(0);
  }

  void insert(const T& data, const S& second) {
    if (empty()) {
      root(new Node(data, second));
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
    return s_null;
  }

  /** Árvore vazia?
   *  \return true se vazia.
   */
  bool empty() const { return root == nullptr; }

 private:
  /** Struct Node.
   *  Nodo da árvore.
   *  Pode ser interpretado como uma árvore ou sub-árvore.
   */
  struct Node {
    int index;

    T data;             /**< Dado. */
    S second;           /** Dado mapeado */

    Node(const data_&, const second_&) {
      data = data_;
      second = second_;
    }

    bool isNull() {
      return data == t_null;
    }

    Node(int index) {
      if (index < 0) {
        throw new std::out_of_range("Index negativo");
      }
      ifstream i(filename);
      int tsize = sizeof(T);
      int ssize = sizeof(S);
      int start = index*(tsize+ssize);
      i.seekg(start);
      i.read(&data, tsize);
      i.seekg(start+tsize);
      i.read(&second, ssize);
    }

    void writeMe(int index) {
      ofstream o(filename);
      int tsize = sizeof(T);
      int ssize = sizeof(S);
      int start = index*(tsize+ssize);
      i.seekg(start);
      i.write(&data, tsize);
      i.seekg(start+tsize);
      i.write(&second, ssize);

    }

    Node* left() {
      return new Node(index*2);
    }

    Node* right() {
      return new Node(index*2 +1);
    }

    Node* parent() {
      return new Node(index/2);
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
     *  \param data dado a inserir.
     */
    void insert(const T& data_, const S& second_) {
      if (data_ > data) {
        Node* r = right();
        Node* l = left();
        if (r.isNull()) {
          right(new Node(data_, second_));
          //right()->parent(this);
          right()->rebalance();
        } else {
          r->insert(data_, second_);
        }
      } else {
        if (left.isNull()) {
          left(new Node(data_, second_));
          //left->parent = this;
          left()->rebalance();
        } else {
          l->insert(data_, second_);
        }
      }
    }

    void rebalance() {
      Node* r = parent;
      while (!(r->parent().isNull())) {
        r = r->parent();
      }
      r->updateHeight();
      for (Node* i = parent(); !i.isNull(); i = i->parent()) {
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
      l = !(left().isNull()) ? left()->height : -1;
      r = !(right().isNull()) ? right()->height : -1;
      return r - l;
    }

    void updateHeight() {
      height = 0;
      Node* l = left();
      Node* r = right();

      if (!l.isNull()) {
        l->updateHeight();
        height += l->height;
      }
      if (!r.isNull()) {
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
      RL->right = R;
      R->parent(RL);
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

  Node* root{nullptr};  /**< Raiz da árvore. */
  std::size_t size_{0}; /**< Tamanho da árvore. */
  std::string filename  /**< FileIO */
  T t_null;
  S s_null;
};

}  // namespace structures

#endif
