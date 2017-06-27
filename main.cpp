#include <iostream>
#include <sstream>
#include <cstring>
#include "helper.h"
#include <bitset>
#include <vector>
extern "C"
{
#include <wb/wbsys.h>
}

typedef unsigned char uchar;
typedef long unsigned int lui;
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
vector<string> vec;

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
	
	if (!data.good()) {
		cerr << "erro ao abrir manpages.dat";
	}
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
  if (!data.good()) {
  	cerr << "erro na leitura de manpages.dat";
  }
  data.seekg(r.start);
  char content[r.length];
  data.read(content, r.length);
	close_seg(db, 0);
	data.close();
  return content;
}

bool check_bit (char* array, unsigned int bit_pos) {
  unsigned int index = bit_pos / 8;
  unsigned int remainder = bit_pos % 8;
  uchar mask = 1 << (7-remainder);
  return array[index] & mask;
}
void add_bit(char* array, unsigned int bit_pos) {
  unsigned int index = bit_pos / 8;
  unsigned int remainder = bit_pos % 8;
  uchar add = 1 << (7-remainder);
  array[index] = array[index] | add;
}

void write_vec() {
	ofstream o("bitmatrix.dat", ios::trunc);
	if (!o.good()) {
		cerr << "erro na escrita de bitmatrix.dat";
	}
	for (vector<string>::iterator it = vec.begin(); it != vec.end(); ++it) {
		o << *it << endl;
	}
	o.close();
}

void read_vec() {
	ifstream i("bitmatrix.dat");
	if (!i.good()) {
		cerr << "erro na leitura de bitmatrix.dat";
	}
	string line;
	vec.clear();
	while (getline(i, line)) {
		vec.push_back(line);
	}
	i.close();
}
void write_secondary (int num_files, char** filenames) {
	db = make_seg((uchar*) "secondary_tree.db", 2048);
	handle = create_db(db, 'T', (uchar*) "secondary");
	int size = num_files/8 + 1;
	lui index = 0;
	vec.clear();
	for (int file = 1; file < num_files; ++file) {
		ifstream i(filenames[file]);
		string word;
		string filename = get_file_name(filenames[file]);
		if ((file *1000 / num_files) % 10 == 0) {
		cout << " (" << (file *100 / num_files) << "%)\r" << flush;
		}
		uchar* fn = (uchar*) filename.c_str();
		while (i >> word) {
			if (!is_stop_word(word)) {
				uchar* cword = (uchar*) word.c_str();
				uchar buffer[256];
				int code = bt_get(handle, cword, word.size(), buffer);
				if (code < 0) {
          char bitset[size] = {0};
          add_bit(bitset, file);
          string s(bitset, size);
          vec.push_back(s);
          lui copy = index;
					int status = bt_write(handle, cword, word.size(), (uchar*) &copy, sizeof(lui));
					
					index++;
					if (status < 0) {
						cerr << "Erro de escrita." << endl;
					}
				} else {
					lui index = *((lui*) buffer);
					char* bitline = (char*) vec[index].c_str();
					if (!check_bit(bitline, file)) {
            add_bit(bitline, file);
					}
				}
			}
		}
		i.close();

	}
	write_vec();
	close_seg(db, 0);
	cout << "Árvore secundária salva." << endl;
}


string read_secondary(int num_files, char** filenames, string word, string word2) {
	read_vec();
	db = open_seg((uchar*) "secondary_tree.db", true);
	handle = open_db(db, (uchar*) "secondary");

	int size = num_files/8 + 1;
	char* bitline, *bitline2;
	ifstream i("inverted.dat");
	string output;
	uchar buffer[256];
	uchar buffer2[256];
	int code = bt_get(handle, (uchar*) word.c_str(), word.size(), buffer);
	int code2 = bt_get(handle, (uchar*) word2.c_str(), word2.size(), buffer2);
	if (code < 0 && code2 < 0) {
		output = "Nenhum arquivo encontrado";
	} else {
		lui index = *((lui*) buffer);
		lui index2 = *((lui*) buffer2);
		bitline =  (char*) vec[index].c_str();
		bitline2 =  (char*) vec[index2].c_str();
		for (int file = 1; file < num_files; file++) {
			bool inFirst = code >= 0 && check_bit(bitline, file-1);
			bool inSecond = code2 >= 0 && check_bit(bitline2, file-1);

		if (inFirst && inSecond) {
			string filename(filenames[file]);
			filename = get_file_name(filename);
			output += filename + "\n";
		}
	}
	}
	i.close();
	close_seg(db, 0);
	return output;
}

/*string list_search (int num_files, char** filenames, char* bitline1, char* bitline2, bool two) {
	string output = "";
	if (bitline1 == nullptr || bitline2 == nullptr) {
		return "Nenhum arquivo encontrado.";
	}
	for (int file = 1; file < num_files; file++) {
		bool inFirst = check_bit(bitline1, file);
		bool inSecond = check_bit(bitline2, file) || !two;
		if (inFirst && inSecond) {
			string filename(filenames[file]);
			filename = get_file_name(filename);
			output += filename + "\n";
		}
	}
	return output;
}*/
void dialog(int argc, char** argv) {
  cout << "Escolha uma opção:" << endl
  << "(1) Criar árvore primária" << endl
  << "(2) Criar árvore secundária" << endl
  << "(3) Buscar por nome de arquivo" << endl
  << "(4) Buscar por palavras" << endl
  << "(5) Buscar por duas palavras" << endl;
 
	string answer;
	cin >> answer;
	cout << endl << "-------------------------" << endl;
	switch ((int) answer.c_str()[0]) {
	case '1':
		write_primary(argc, argv, "manpages.dat");
		break;
	case '2':
		write_secondary(argc, argv);
		break;
	case '3':
		cout << "Digite o nome do arquivo sem extensão." << endl;
		cin >> answer;
		cout << "Conteúdo do arquivo " << answer << ":\n" << read_primary(answer, "manpages.dat") << endl;
		break;
	case '4': {
		cout << "Digite a palavra a buscar." << endl;
		cin >> answer;
		string list = read_secondary(argc, argv, answer, answer);
		cout << "Lista de arquivos com essa palavra:\n" <<  list << endl;
		break;
		}
	case '5': {
		cout << "Digite duas palavras para buscar." << endl;
		string word1, word2;
		cin >> word1;
		cin >> word2;
		string dlist = read_secondary(argc, argv, word1, word2);
		cout << "Lista de arquivos com essas palavras:\n" <<  dlist << endl;
		break;
		}
	default:
		cout << "Entrada inválida." << endl;
		break;
	}
	cout << endl << "-------------------------" << endl;
	dialog(argc, argv);
}
int main (int argc, char** argv) {
	init_wb(75, 150, 4096);
	cout << (argc-1) << " arquivos." << endl;
  load_stop_words("stop_words.txt");


  dialog(argc, argv);
	final_wb();
  return 0;
}
