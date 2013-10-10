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

