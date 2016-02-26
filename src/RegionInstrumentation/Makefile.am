LLVMCXXFLAGS    := $(shell "$(LLVM_CONFIG)" --cxxflags)
libRegionInstrumentation_la_CXXFLAGS = $(LLVMCXXFLAGS)


lib_LTLIBRARIES = libRegionInstrumentation.la
libRegionInstrumentation_la_SOURCES = RegionInstrumentation.cpp LoopRegionInstrumentation.cpp OmpRegionInstrumentation.cpp
libRegionInstrumentation_la_LDFLAGS =
