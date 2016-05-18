LLVMCXXFLAGS    := $(shell "$(LLVM_CONFIG)" --cxxflags)
libcere_regioninstrumentation_la_CXXFLAGS = $(LLVMCXXFLAGS)


lib_LTLIBRARIES = libcere_regioninstrumentation.la
libcere_regioninstrumentation_la_SOURCES = RegionInstrumentation.cpp LoopRegionInstrumentation.cpp OmpRegionInstrumentation.cpp
libcere_regioninstrumentation_la_LDFLAGS =
