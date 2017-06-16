#ifndef HELPER_H
#define HELPER_H

#include <fstream>
#include <string>
#include <cerrno>
#include <stdexcept>
namespace helper {

std::string get_file_contents(std::string filename);
std::string get_file_name(std::string filename);
long unsigned int get_file_size(std::string filename);
}

#endif
