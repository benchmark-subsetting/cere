CFLAGS=-g -fPIC
CC=clang

all: libdump.so

htable.o: ../htable/htable/htable.c
	$(CC) $(CFLAGS) -fPIC -c ../htable/htable/htable.c

libdump.so: mallochook.o dump.o htable.o snoop.o
	clang -shared -ldl -o $@ mallochook.o dump.o htable.o snoop.o

clean:
	rm -f test test.c libdump.so *.S *.memdump *.o core.map original patched
	rm -rf dump/
