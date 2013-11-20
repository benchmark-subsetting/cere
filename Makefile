SUBDIRS=" libs/dragonegg-3.3.src/" 

all: 
	@echo
	@echo "Please configure your src/llvm-3.3.src"
	@echo "You can cancel this Make by pressing CTRL-C"
	@read PAUSE
	$(MAKE) -C src/llvm-3.3.src/
	sudo $(MAKE) -C src/llvm-3.3.src/ install
	$(MAKE) -C libs/dragonegg-3.3.src/
	cp libs/dragonegg-3.3.src/dragonegg.so libs/dragonegg.so
	$(MAKE) -C src/LoopExtractorAll/
	$(MAKE) -C src/LoopInstrumentation/
	$(MAKE) -C src/LoopManager/
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
