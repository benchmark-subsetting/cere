CC=clang
CFLAGS=-O3 -Wall

all:
	$(CC) $(CFLAGS) -c rdtsc.c
	$(CC) $(CFLAGS) -c htable/htable/htable.c
	ar -r rdtsc.a htable.o rdtsc.o 

test: all
	$(CC) $(CFLAGS) test.c rdtsc.a -o test

clean:
	rm -f *.o

distclean: clean
	rm -f *.a test

