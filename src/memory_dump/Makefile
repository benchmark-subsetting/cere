CFLAGS=-g -fPIC -O0
CC=clang

all: libdump.so libload.a 

htable.o: ../htable/htable/htable.c
	$(CC) $(CFLAGS) -c ../htable/htable/htable.c

warm.o : warm.c
	$(CC) -c -g -fPIC -O3 -o $@ $^

libload.a: replay.o warm.o
	ar -r libload.a $^

libdump.so: mallochook.o dump.o counters.o htable.o
	clang -shared -ldl -o $@ $^

clean:
	rm -f *.o *.so 
	rm -rf dump/
