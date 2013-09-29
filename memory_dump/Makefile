#FLAGS=-g

all: test

test: test.c dump.o
	clang $(FLAGS) -o test test.c dump.o

dump.o: dump.c
	clang $(FLAGS) -c dump.c 

clean:
	rm -f test dump.o
