/* Copyright [2017] <Gabriel Müller>
 * array_list.h
 */

#ifndef STRUCTURES_ARRAY_LIST_H
#define STRUCTURES_ARRAY_LIST_H

#include <cstdint>
#include <stdexcept>

namespace structures {

template<typename T>

/** Implementação de lista com vetor de
 *  um tipo genérico.
 */
class ArrayList {
 public:
    /** Construtor sem parâmetros.
     *  Cria uma lista de tamanho padrão.
     */
    ArrayList();

    /** Construtor de lista.
     * \param max um inteiro sem sinal que representa o tamanho máximo da lista.
     */
    explicit ArrayList(std::size_t max_size);

    /** Destrutor de lista.
     *  Deleta o espaço de memória reservado para 'contents'.
     */
    ~ArrayList();

    /** Limpa a lista.
     *  Reseta a lista, removendo todos os dados.
     */
    void clear();

    /** Adiciona um dado à parte de trás da lista.
     *  \param data Endereço do dado de tipo genérico a ser adicionado.
     */
    void push_back(const T& data);

    /** Adiciona um dado na frente da lista.
     *  \param data Endereço do dado de tipo genérico a ser adicionado.
     */
    void push_front(const T& data);

    /** Insere um dado na lista na posição especificada.
     *  Desloca outros dados a fim de manter a continuidade.
     *  \param data dado de tipo genérico a ser inserido.
     *  \param index posição na lista a inserir.
     */
    void insert(const T& data, std::size_t index);

    /** Insere um dado na lista de acordo com o ordenamento dos elementos.
     *  \param data dado a ser inserido.
     */
    void insert_sorted(const T& data);

    /** Remove o dado na posição especificada.
     *  \param index posição do dado a remover.
     *  \return o dado removido.
     */
    T pop(std::size_t index);

    /** Remove o dado de trás da lista.
     *  \return o dado de trás removido.
     */
    T pop_back();

    /** Remove o dado da frente da lista.
     *  Todos outros dados são deslocados para frente.
     *  \return o dado da frente removido.
     */
    T pop_front();

    /** Remove um dado.
     * \param data dado a ser removido.
     */
    void remove(const T& data);

    /** Está cheia?
     *  \return 'true' se a lista estiver cheia.
     */
    bool full() const;

    /** Está vazia?
     *  \return 'true' se a lista estiver vazia.
     */
    bool empty() const;

    /** Contém o dado especificado?
     *  \param data dado em questão.
     *  \return 'true' se a lista contiver o dado.
     */
    bool contains(const T& data) const;

    /** Posição do dado especificado no vetor.
     *  \param data dado em questão.
     *  \return posição do dado, ou tamanho + 1 se o dado não for achado.
     *  Inteiro sem sinal.
     */
    std::size_t find(const T& data) const;

    /** Tamanho atual da lista.
     *  \return quantos dados a lista contém no momento. Inteiro sem sinal.
     */
    std::size_t size() const;

    /** Tamanho máximo da lista.
     *  \return número máximo de dados que a lista pode conter.
     *  Inteiro sem sinal.
     */
    std::size_t max_size() const;

    /** Dado na posição especificada.
     *  \param index posição no vetor.
     *  \return dado na posição especificada.
     */
    T& at(std::size_t index);

    /** Dado na posição especificada com operador sobrecarregado.
     *  \param index posição no vetor.
     *  \return dado na posição especificada.
     */
    T& operator[](std::size_t index);

    /** Dado na posição especificada apenas para leitura.
     *  \param index posição no vetor.
     *  \return dado na posição especificada.
     */
    const T& at(std::size_t index) const;

    /** Dado na posição especificada com operador sobrecarregado apenas leitura.
     *  \param index posição no vetor.
     *  \return dado na posição especificada.
     */
    const T& operator[](std::size_t index) const;

 private:
    T* contents;  /**< Vetor de dados da lista. */
    std::size_t size_;  /**< Endereço do fim da lista. '0' se lista vazia. */
    std::size_t max_size_;  /**< Tamanho máximo. Inteiro sem sinal. */
    auto static const DEFAULT_SIZE = 10u;  /**< Tamanho padrão da lista. */
};

template<typename T>
ArrayList<T>::ArrayList() {	 	  	 	     	 	      		      	     	  	 	
    max_size_ = DEFAULT_SIZE;
    size_ = 0;
    contents = new T[max_size_];
}

template<typename T>
ArrayList<T>::ArrayList(std::size_t max_size) {
    max_size_ = max_size;
    size_ = 0;
    contents = new T[max_size_];
}

template<typename T>
ArrayList<T>::~ArrayList() {
    delete[] contents;
}

template<typename T>
void ArrayList<T>::clear() {
    size_ = 0;
}

template<typename T>
void ArrayList<T>::push_back(const T& data) {
    insert(data, size_);
}

template<typename T>
void ArrayList<T>::push_front(const T& data) {
    insert(data, 0);
}

template<typename T>
void ArrayList<T>::insert(const T& data, std::size_t index) {
    if (full()) {
        throw std::out_of_range("Lista cheia.");
    } else if (index > size_) {	 	  	 	     	 	      		      	     	  	 	
        throw std::out_of_range("Posição não existe");
    } else {
        for (int i = size_; i > index; i--) {
            contents[i] = contents[i - 1];
        }
        contents[index] = data;
        size_++;
    }
}

template<typename T>
void ArrayList<T>::insert_sorted(const T& data) {
    if (full()) {
        throw std::out_of_range("Lista cheia.");
    } else {
        std::size_t i = 0;
        while (i < size_ && data > contents[i]) {
            i++;
        }
        insert(data, i);
    }
}

template<typename T>
T ArrayList<T>::pop(std::size_t index) {
    if (empty()) {
        throw std::out_of_range("Lista vazia.");
    } else if (index >= size_) {
        throw std::out_of_range("Posição não existe");
    } else {
        T output = contents[index];
        for (int i = index; i < size_ - 1; i++) {
            contents[i] = contents[i + 1];
        }
        size_--;
        return output;
    }	 	  	 	     	 	      		      	     	  	 	
}

template<typename T>
T ArrayList<T>::pop_back() {
    return pop(size_ - 1);
}

template<typename T>
T ArrayList<T>::pop_front() {
    return pop(0);
}

template<typename T>
void ArrayList<T>::remove(const T& data) {
    pop(find(data));
}

template<typename T>
bool ArrayList<T>::full() const {
    return size_ == max_size_;
}

template<typename T>
bool ArrayList<T>::empty() const {
    return size_ == 0;
}

template<typename T>
bool ArrayList<T>::contains(const T& data) const {
    return find(data) != size_;
}

template<typename T>
std::size_t ArrayList<T>::find(const T& data) const {
    std::size_t output;
    if (empty()) {	 	  	 	     	 	      		      	     	  	 	
        output = size_;
    } else {
        std::size_t i = 0;

        while (i < size_ && contents[i] != data) {
            i++;
        }

        if (i >= size_) {
            output = size_;
        } else {
            output = i;
        }
    }
    return output;
}

template<typename T>
std::size_t ArrayList<T>::size() const {
    return size_;
}

template<typename T>
std::size_t ArrayList<T>::max_size() const {
    return max_size_;
}

template<typename T>
T& ArrayList<T>::at(std::size_t index) {
    if (index >= size_) {
        throw std::out_of_range("Posição está fora da lista.");
    } else {
        return contents[index];
    }
}

template<typename T>
T& ArrayList<T>::operator[](std::size_t index) {	 	  	 	     	 	      		      	     	  	 	
    return at(index);
}

template<typename T>
const T& ArrayList<T>::at(std::size_t index) const {
    return at(index);
}

template<typename T>
const T& ArrayList<T>::operator[](std::size_t index) const {
    return at(index);
}

}  // namespace structures

#endif
