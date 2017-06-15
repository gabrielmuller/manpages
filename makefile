ARGS=

compilerun:
	make -s compile
	make -s run
compile:
	g++ main.cpp -o main.o -std=c++14
run:
	./main.o $(ARGS)
