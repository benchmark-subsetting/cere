LLVMCXXFLAGS    := $(shell "$(LLVM_CONFIG)" --cxxflags)
libcere_regionoutliner_la_CXXFLAGS = $(LLVMCXXFLAGS)


lib_LTLIBRARIES = libcere_regionoutliner.la
libcere_regionoutliner_la_SOURCES = RegionExtractor.cpp LoopRegionOutliner.cpp OmpRegionOutliner.cpp
libcere_regionoutliner_la_LDFLAGS =
