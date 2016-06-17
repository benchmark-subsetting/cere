LLVMCXXFLAGS    := $(shell "$(LLVM_CONFIG)" --cxxflags)
libcere_regiondump_la_CXXFLAGS = $(LLVMCXXFLAGS)


lib_LTLIBRARIES = libcere_regiondump.la
libcere_regiondump_la_SOURCES = RegionDump.cpp LoopRegionDump.cpp OmpRegionDump.cpp RegionDump.h
