SUBDIRS=" libs/dragonegg-3.3.src/" 

all: 
	CC=gcc-4.7 $(MAKE) -C libs/dragonegg-3.3.src/
	cp libs/dragonegg-3.3.src/dragonegg.so libs/dragonegg.so
	$(MAKE) -C src/
	$(MAKE) -C src/memory_dump/
	$(MAKE) -C src/rdtsc/

clean:
	$(MAKE) -C src/llvm-3.3.src clean
	$(MAKE) -C src/LoopExtractorAll/ clean
	$(MAKE) -C src/LoopInstrumentation/ clean
	$(MAKE) -C src/LoopManager/ clean
	$(MAKE) -C src/rdtsc/ distclean
	$(MAKE) -C tests/test_00/ veryclean
	$(MAKE) -C tests/test_01/ veryclean
	$(MAKE) -C tests/test_02/ veryclean
	$(MAKE) -C tests/test_03/ veryclean
