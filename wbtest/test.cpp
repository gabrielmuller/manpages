

#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
extern "C"
{
#include <wb/wbsys.h>
}

typedef unsigned char uchar;
using namespace std;


/*inline void uchar_out(uchar* str, int len, ostream *stream = 0)
{
	if (stream == 0)
		stream = &cout;
	char *t = new char[len + 1];
	int i;
	for (i = 0; i < len; ++i)
		t[i] = str[i];
	t[i] = 0;
	(*stream) << t << endl;
	delete[] t;
}*/
int main(int argc, char *argv[])
{
	init_wb(75, 150, 4096);
	SEGD* db = make_seg((uchar*) "btree.db", 2048);
	HAND* handle = create_db(db, 'T', (uchar*) "primaria");

	uchar cc = (uchar) 55;
	int r = bt_write(handle, (uchar*) "top", 4, &cc, 1);

	uchar c[256];
	int s = bt_get(handle, (uchar*) "top", 4, c);
	int i = 10;
	cout << (int) c[0] << endl;
	return 0;
}
