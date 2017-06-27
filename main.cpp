#include <iostream>
#include <sstream>
#include <cstring>
#include "helper.h"

extern "C"
{
#include <wb/wbsys.h>
}

typedef unsigned char uchar;
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


HAND* handle;
SEGD* db;


//  cria registro primário
//  num_files é o número de arquivos + 1 por causa do argc
void write_primary (int num_files, char** filenames, string output) {
	db = make_seg((uchar*) "primary_tree.db", 2048);
	handle = create_db(db, 'T', (uchar*) "primary");
  long unsigned int total_size = 0;

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
    r.start = index;
    r.length = length;

		//insere na árvore
		uchar* fn = (uchar*) filename.c_str();
    bt_write(handle, fn, filename.size(), (uchar*) &r, sizeof(str_range));

    //  atualiza index para próximo arquivo
    index += length;

    //  salva conteúdo desse arquivo
    data << content;
  }
  //TO-DO: serializar árvore
  //por enquanto só tem o texto.
  data.close();
	close_seg(db, 0);
  cout << "Árvore primária salva." << endl;
}

string read_primary (string filename, string input) {
	db = open_seg((uchar*) "primary_tree.db", true);
	handle = open_db(db, (uchar*) "primary");

	uchar buffer[sizeof(str_range)];
  //  acha posição do arquivo em all_content
  bt_get(handle, (uchar*) filename.c_str(), filename.size(), buffer);
	str_range r = *((str_range*) buffer);
	cout << r.start << " " << r.length << "\n";
  //lê a parte relevante
  ifstream data (input);
  data.seekg(r.start);
  char content[r.length];
  data.read(content, r.length);
	close_seg(db, 0);
  return content;
}

bool contains_line (string source, string line) {
	istringstream s(source);
	string l;
	while (getline(s, l)) {
		if (line == l) {
			return true;
		}
	} 
	return false;
}

void write_secondary (int num_files, char** filenames) {
	db = make_seg((uchar*) "secondary_tree.db", 2048);
	handle = create_db(db, 'T', (uchar*) "secondary");
	
	for (int file = 1; file < num_files; ++file) {
		ifstream i(filenames[file]);
		string word;
		string filename = get_file_name(filenames[file]);
		uchar* fn = (uchar*) filename.c_str();
		while (i >> word) {
			if (!is_stop_word(word)) {
				uchar* cword = (uchar*) word.c_str();
				uchar buffer[10000];
				int code = bt_get(handle, cword, word.size(), buffer);
				if (code < 0) {
					bt_write(handle, cword, word.size(), fn, strlen((char*)fn));
				} else {

					string buff((char*)buffer);
					if (!contains_line(buff, filename)) {
						string to_put = buff + "\n" + filename;
						if (word == "<beginning") { cout << to_put << endl; }
						bt_put(handle, cword, word.size(), (uchar*) to_put.c_str(), to_put.size());
					}		
				}
			}
		}
	}
	close_seg(db, 0);
	cout << "Árvore secundária salva." << endl;
}



string read_secondary(string word) {
	db = open_seg((uchar*) "secondary_tree.db", true);
	handle = open_db(db, (uchar*) "secondary");
	uchar buffer[10000];
	int code = bt_get(handle, (uchar*) word.c_str(), word.size(), buffer);
	string output((char*)buffer);
	close_seg(db, 0);
	return output;
}

int main (int argc, char** argv) {
	init_wb(75, 150, 4096);
  load_stop_words("stop_words.txt");
	write_primary(argc, argv, "manpages.dat");
	write_secondary(argc, argv);
	cout << "Procurar na árvore primária ou secundária? [1/2]" << endl;
	string answer;
	cin >> answer;
	if (answer == "1") {
		cout << "Digite o nome do arquivo sem extensão." << endl;
		cin >> answer;
		cout << "Conteúdo do arquivo " << answer << ":\n" << read_primary(answer, "manpages.dat") << endl;
	} else if (answer == "2") {
		cout << "Digite a palavra a buscar." << endl;
		cin >> answer;
		cout << "Lista de arquivos com essa palavra:\n" << read_secondary(answer) << endl; 
	} else {
		cout << "Input inválido." << endl;
	}
	final_wb();
  return 0;
}
