CC=clang++
CFLAGS=-O3 -Wall

all:
	$(CC) $(CFLAGS) -c rdtsc.cpp
	ar -r rdtsc.a rdtsc.o

clean:
	rm -f *.o

distclean: clean
	rm -f *.a test

