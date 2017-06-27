ARGS=
PARAMS=-std=c++14 -lwb
INPUT=pages_little
compilerun:
	make -s compile
	make -s run
compile:
	g++ *.cpp *.h -o main.o $(PARAMS)
run:
	./main.o $(ARGS) $(INPUT)/*.txt
debug:
	g++ -g *.cpp *.h -o main.o $(PARAMS)
	gdb main.o
