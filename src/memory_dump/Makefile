CFLAGS=-g -fPIC -O2
CC=clang

all: libdump.so libload.a 

htable.o: ../htable/htable/htable.c
	$(CC) $(CFLAGS) -c ../htable/htable/htable.c

libload.a: replay.o
	ar -r libload.a replay.o

libdump.so: mallochook.o dump.o counters.o htable.o
	clang -shared -ldl -o $@ $^

clean:
	rm -f *.o *.so 
	rm -rf dump/
