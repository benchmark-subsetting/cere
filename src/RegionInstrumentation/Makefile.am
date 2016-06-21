LLVMCXXFLAGS    := $(shell "$(LLVM_CONFIG)" --cxxflags | sed s/-Wcovered-switch-default// | sed s/-Werror=date-time//)
libcere_regioninstrumentation_la_CXXFLAGS = $(LLVMCXXFLAGS)


lib_LTLIBRARIES = libcere_regioninstrumentation.la
libcere_regioninstrumentation_la_SOURCES = RegionInstrumentation.cpp LoopRegionInstrumentation.cpp OmpRegionInstrumentation.cpp RegionInstrumentation.h
