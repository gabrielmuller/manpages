#include <iostream>
#include <map>
#include <cstring>
#include "helper.h"

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
//  árvore que mapeia nome do arquivo a dois inteiros
//  int[0]: posição do conteúdo do arquivo no texto
//  int[1]: tamanho do conteúdo
//variável global porque ainda não tá serializado
map<string, str_range>  primary_tree;

//  cria registro primário
//  num_files é o número de arquivos + 1 por causa do argc
void write_primary (int num_files, char** filenames, std::string output) {
  long unsigned int total_size = 0;

  //  calcula tamanho total de todos arquivos
  for (auto i = 1; i < num_files; i++) {
    total_size += get_file_size(filenames[i]);
  }

  //  texto de todos arquivos vai aqui
  char all_content[total_size];

  //  posição desse arquivo all_content
  long unsigned int index = 0;

  for (auto i = 1; i < num_files; i++) {

    //  nome do arquivo sem extensão etc
    string filename = get_file_name(filenames[i]);

    cout << "Inserindo entrada " << filename << " na árvore." << endl;
    auto size = get_file_size(filenames[i]);
    str_range r;
    r.start = index;
    r.length = size;
    primary_tree.insert(pair<string, str_range>(filename, r));

    //  atualiza index para próximo arquivo
    index += get_file_size(filenames[i]);

    //  concatena conteúdo desse arquvio
    strcat(all_content, get_file_contents(filenames[i]).c_str());
  }

  //  cria registro primário
  ofstream data (output);

  //TO-DO: serializar árvore
  //por enquanto só tem o texto.
  data << all_content;
  data.close();
  cout << "Arquivo " << output << " escrito." << endl;
}

std::string read_primary (std::string filename, std::string input) {

  //  acha posição do arquivo em all_content
  map<string, str_range>::iterator it = primary_tree.find(filename);
  if (it == primary_tree.end()) {
    throw std::runtime_error(
      "Arquivo não pôde ser lido.");
  }
  str_range r = it->second;

  //lê a parte relevante
  ifstream data (input);
  data.seekg(r.start);
  char content[r.length];
  data.read(content, r.length);

  cout << filename << " em " << input << ":" << endl << content << endl;
}

int main (int argc, char** argv) {
  write_primary(argc, argv, "manpages.dat");
  read_primary("asa.1", "manpages.dat");

  return 0;
}
