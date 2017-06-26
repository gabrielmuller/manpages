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
  /** Destrutor.
   *
   */
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
  void insert(const T& data, const S& second) {
    if (empty()) {
      root = new Node(data, second);
    } else {
      root->insert(data, second);
    }
    size_++;
  }

  /** enjambreitioon
   *
   */
  S find(T data) {
    Node* it = root;

    while (it != nullptr) {
      T itd = it->data;

      if (itd == data) {
        return it->second;
      } else if (itd > data) {
        it = it->left;
      } else if (itd < data) {
        it = it->right;
      }
    }

    //não achou
    S fail;
    return fail;
  }

  /** Remove dado.
   *  \param data dado a remover.
   */
  void remove(const T& data) {
    if (!contains(data)) {
      throw std::out_of_range("Dado a deletar não existe");
    }
    root->remove(data);
    size_--;
  }

  /** Contém dado?
   *  \param data dado.
   *  \return true se dado pertence à árvore.
   */
  bool contains(const T& data) const {
    if (empty()) {
      throw std::out_of_range("Árvore vazia");
    }
    return root->contains(data);
  }

  /** Árvore vazia?
   *  \return true se vazia.
   */
  bool empty() const { return root == nullptr; }

  /** Tamanho.
   *  \return número de elementos da árvore.
   */
  std::size_t size() const { return size_; }

  /** Pré-ordem.
   *  \return lista de vetor em pré-ordem.
   */
  ArrayList<T> pre_order() const {
    if (empty()) {
      throw std::out_of_range("Árvore vazia");
    }
    ArrayList<T> result = ArrayList<T>(size());
    root->pre_order(result);
    return result;
  }

  /** Em-ordem.
   *  \return lista de vetor em-ordem.
   */
  ArrayList<T> in_order() const {
    if (empty()) {
      throw std::out_of_range("Árvore vazia");
    }
    ArrayList<T> result = ArrayList<T>(size());
    root->in_order(result);
    return result;
  }

  /** Pós-ordem.
   *  \return lista de vetor em pós-ordem.
   */
  ArrayList<T> post_order() const {
    if (empty()) {
      throw std::out_of_range("Árvore vazia");
    }
    ArrayList<T> result = ArrayList<T>(size());
    root->post_order(result);
    return result;
  }

 private:
  /** Struct Node.
   *  Nodo da árvore.
   *  Pode ser interpretado como uma árvore ou sub-árvore.
   */
  struct Node {
    /** Construtor por dado.
     *  \param data_ dado.
     */
    explicit Node(const T& data_, const S& second_) {
      data = data_;
      second = second_;
      right = nullptr;
      left = nullptr;
      parent = nullptr;
    }

    void delete_all() {
      if (left != nullptr) {
        left->delete_all();
      }
      if (right != nullptr) {
        right->delete_all();
      }
      delete this;
    }

    T data;             /**< Dado. */
    S second;           /** Dado mapeado */
    Node* left;         /**< Ponteiro para nodo à esquerda. */
    Node* right;        /**< Ponteiro para nodo à direita. */
    Node* parent;       /**< Ponteiro para nodo-pai. (nulo se raiz) */
    std::size_t height; /**< Altura do nodo. */

    /** Insere dado.
     *  \param data dado a inserir.
     */
    void insert(const T& data_, const S& second_) {
      if (data_ > data) {
        if (right == nullptr) {
          right = new Node(data_, second_);
          right->parent = this;
          right->rebalance();
        } else {
          right->insert(data_, second_);
        }
      } else {
        if (left == nullptr) {
          left = new Node(data_, second_);
          left->parent = this;
          left->rebalance();
        } else {
          left->insert(data_, second_);
        }
      }
    }

    void rebalance() {
      Node* r = parent;
      while (r->parent != nullptr) {
        r = r->parent;
      }
      r->updateHeight();
      for (Node* i = parent; i != nullptr; i = i->parent) {
        if (i->factor() == 2) {
          if (i->right->factor() == 1) {
            simpleLeft();
          } else if (i->right->factor() == -1) {
            doubleLeft();
          }
        } else if (i->factor() == -2) {
          if (i->left->factor() == -1) {
            simpleRight();
          } else if (i->right->factor() == 1) {
            doubleRight();
          }
        }
      }
    }

    int factor() {
      int l, r;
      l = left != nullptr ? left->height : -1;
      r = right != nullptr ? right->height : -1;
      return r - l;
    }
    /** Remove dado.
     *  \param data dado a remover.
     */
    void remove(const T& data_) {
      if (data_ > data) {
        right->remove(data_);
      } else if (data_ < data) {
        left->remove(data_);
      } else if (data == data_) {
        // achou elemento a deletar
        remove_me();
      }
    }

    /** Remove este nodo.
     *  Auxilia remove()
     */
    void remove_me() {
      if (right == nullptr && left == nullptr) {
        // nenhum filho
        if (parent != nullptr) {
          // tem pai
          if (parent->right == this) {
            parent->right = nullptr;
          } else {
            parent->left = nullptr;
          }
          parent->rebalance();
        }
        delete this;
      } else if (right != nullptr && left != nullptr) {
        // dois filhos
        if (right->right == nullptr && right->left == nullptr) {
          // direita não tem filhos
          this->data = right->data;
          right->remove_me();
        } else if (right->left != nullptr) {
          // direita tem filho à esquerda
          this->data = right->left->data;
          right->left->remove_me();
        } else {
          // direita tem só filho da direita
          this->data = right->right->data;
          right->right->remove_me();
        }
      } else {
        // um filho
        if (left != nullptr) {
          // esquerda existe
          this->data = left->data;
          left->remove_me();
        } else {
          // direita existe
          this->data = right->data;
          right->remove_me();
        }
      }
    }

    /** Contém dado?
     *  \param data dado.
     *  \return true se dado pertence à árvore.
     */
    bool contains(const T& data_) const {
      if (data == data_) {
        return true;
      } else if (right == nullptr && left == nullptr) {
        return false;
      } else {
        bool l = false;
        bool r = false;
        if (left != nullptr) {
          l = left->contains(data_);
        }
        if (right != nullptr) {
          r = right->contains(data_);
        }
        return l || r;
      }
    }

    void updateHeight() {
      height = 0;
      if (left != nullptr) {
        left->updateHeight();
        height += left->height;
      }
      if (right != nullptr) {
        right->updateHeight();
        height += right->height;
      }
    }

    void simpleLeft() {
      Node* R = right;
      Node* RL = right->left;
      right = RL;
      if (RL != nullptr) {
        RL->parent = this;
      }
      R->left = this;
      parent = R;
    }

    void simpleRight() {
      Node* L = left;
      Node* LR = left->right;
      left = LR;
      if (LR != nullptr) {
        LR->parent = this;
      }
      L->right = this;
      parent = L;
    }

    void doubleLeft() {
      Node* R = right;
      Node* RL = right->left;
      Node* RLR = right->left->right;
      R->left = RLR;
      if (RLR != nullptr) {
        RLR->parent = R;
      }
      RL->right = R;
      R->parent = RL;
      Node* RLL = RL->left;
      right = RLL;
      if (RLL != nullptr) {
        RLL->parent = this;
      }
      RL->left = this;
      RL->parent = this;
    }

    void doubleRight() {
      Node* L = left;
      Node* LR = left->right;
      Node* LRL = left->right->left;
      L->right = LRL;
      if (LRL != nullptr) {
        LRL->parent = L;
      }
      LR->left = L;
      L->parent = LR;
      Node* LRR = LR->right;
      left = LRR;
      if (LRR != nullptr) {
        LRR->parent = this;
      }
      LR->right = this;
      LR->parent = this;
    }

    /** Pré-ordem.
     *  \return lista de vetor em pré-ordem.
     */
    void pre_order(ArrayList<T>& v) const {
      v.push_back(data);
      if (left != nullptr) {
        left->pre_order(v);
      }
      if (right != nullptr) {
        right->pre_order(v);
      }
    }

    /** Em-ordem.
     *  \return lista de vetor em-ordem.
     */
    void in_order(ArrayList<T>& v) const {
      if (left != nullptr) {
        left->in_order(v);
      }
      v.push_back(data);
      if (right != nullptr) {
        right->in_order(v);
      }
    }

    /** Pós-ordem.
     *  \return lista de vetor em pós-ordem.
     */
    void post_order(ArrayList<T>& v) const {
      if (left != nullptr) {
        left->post_order(v);
      }
      if (right != nullptr) {
        right->post_order(v);
      }
      v.push_back(data);
    }
  };

  Node* root{nullptr};  /**< Raiz da árvore. */
  std::size_t size_{0}; /**< Tamanho da árvore. */
};

}  // namespace structures

#endif
