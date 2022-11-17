CC = cerec

CFLAGS = -O3 -g -funroll-loops -fprefetch-loop-arrays -fpermissive -fno-exceptions -DPARSEC_VERSION=3.0-beta-20150206 -fopenmp -DNCO=4 -c blackscholes.m4.cpp -o blackscholes.m4.o 
LFLAGS = -fopenmp -lprofiler -lm -L/usr/lib64 -L/usr/lib -Wl,--hash-style=both,--as-needed  ./blackscholes.m4.o -o blackscholes


all:
	$(CC) ${MODE} $(CFLAGS)
	$(CC) ${MODE} $(LFLAGS)

clean:
	rm -rf blackscholes blackscholes.m4.o

veryclean: clean
	- rm -rf .cere/
	- rm -f *.o realmain.c cere.log *.csv lel_bin test.log out cere.json *.ll
