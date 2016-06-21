LLVMCXXFLAGS    := $(shell "$(LLVM_CONFIG)" --cxxflags | sed s/-Wcovered-switch-default// | sed s/-Werror=date-time//)
libcere_regionoutliner_la_CXXFLAGS = $(LLVMCXXFLAGS)


lib_LTLIBRARIES = libcere_regionoutliner.la
libcere_regionoutliner_la_SOURCES = RegionExtractor.cpp LoopRegionOutliner.cpp OmpRegionOutliner.cpp RegionExtractor.h
