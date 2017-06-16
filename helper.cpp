#ifndef HELPER_CPP
#define HELPER_CPP

#include "helper.h"
#include <fstream>
namespace helper {

//  Conteúdo do arquivo como string.
std::string get_file_contents(std::string filename)
{
  std::ifstream in(filename, std::ios::in | std::ios::binary);
  if (in)
  {
    std::string contents;
    in.seekg(0, std::ios::end);
    contents.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&contents[0], contents.size());
    in.close();
    return(contents);
  }

  throw std::runtime_error("não é possível ler o arquivo.");
}

//  Remove ".txt" do nome do arquivo.
std::string get_file_name(std::string filename) {
  size_t last_dot = filename.find_last_of(".");
  size_t last_slash = filename.find_last_of("/") + 1;
  size_t length = last_dot-last_slash;
  if (last_dot == std::string::npos) {
    last_dot = filename.size();
  }
  if (last_slash == std::string::npos) {
    last_slash = 0;
  }

  return filename.substr(last_slash, length);
}

//  Tamanho do arquivo pelo nome
long unsigned int get_file_size(std::string filename) {
  std::ifstream file (filename, std::ios::ate | std::ios::binary);
  return file.tellg();
}

} //  namespace helper
#endif
