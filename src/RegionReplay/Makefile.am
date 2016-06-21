LLVMCXXFLAGS    := $(shell "$(LLVM_CONFIG)" --cxxflags | sed s/-Wcovered-switch-default// | sed s/-Werror=date-time//)
libcere_regionreplay_la_CXXFLAGS = $(LLVMCXXFLAGS)


lib_LTLIBRARIES = libcere_regionreplay.la
libcere_regionreplay_la_SOURCES = RegionReplay.cpp LoopRegionReplay.cpp OmpRegionReplay.cpp RegionReplay.h
