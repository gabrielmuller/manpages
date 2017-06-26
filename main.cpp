#include <iostream>
#include <map>
#include <cstring>
#include "helper.h"
#include "avl_tree.h"
using namespace std;
using namespace helper;

struct str_range {
  int start;
  int length;

  void operator=(str_range other) {
    start = other.start;
    length = other.length;
  }
};
//  árvore que mapeia nome do arquivo a struct de dois inteiros
//variável global porque ainda não tá serializado
typedef structures::AVLTree<string, str_range> ptree;
ptree primary_tree;
//  cria registro primário
//  num_files é o número de arquivos + 1 por causa do argc
void write_primary (int num_files, char** filenames, std::string output) {
  long unsigned int total_size = 0;

  //  calcula tamanho total de todos arquivos
  /*for (auto i = 1; i < num_files; i++) {
    total_size += get_file_size(filenames[i]);
  }

  //  texto de todos arquivos vai aqui
  char all_content[total_size];*/

  //  posição desse arquivo all_content
  long unsigned int index = 0;

  //  cria registro primário
  ofstream data (output, ios::trunc);

  for (auto i = 1; i < num_files; i++) {

    //  nome do arquivo sem extensão etc
    string filename = get_file_name(filenames[i]);
    string content = get_file_contents(filenames[i]);

    str_range r;
    auto length = content.length();
    cout << "Inserindo entrada " << filename << " na árvore " <<
     " posição " << index << " tamanho "
      << length << "...\n" << flush;
    r.start = index;
    r.length = length;
    primary_tree.insert(filename, r);

    //  atualiza index para próximo arquivo
    index += length;

    //  salva conteúdo desse arquivo
    data << content;
  }
  //TO-DO: serializar árvore
  //por enquanto só tem o texto.
  data.close();
  cout << "Arquivo " << output << " escrito." << endl;
}

std::string read_primary (std::string filename, std::string input) {

  //  acha posição do arquivo em all_content
  str_range r = primary_tree.find(filename);

  //lê a parte relevante
  ifstream data (input);
  data.seekg(r.start);
  char content[r.length];
  data.read(content, r.length);

  cout << "Lendo " << filename << " em " << input << ":\n" << content << endl;
}

int main (int argc, char** argv) {
  load_stop_words("stop_words.txt");
  cout << "hello é conectivo? " << is_stop_word("hello") << endl
  << "you é conectivo? " << is_stop_word("you") << endl;
  write_primary(argc, argv, "manpages.dat");
  read_primary("asa.1", "manpages.dat");

  return 0;
}
