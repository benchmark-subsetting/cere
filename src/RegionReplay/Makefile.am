LLVMCXXFLAGS    := $(shell "$(LLVM_CONFIG)" --cxxflags | sed s/-Wcovered-switch-default// | sed s/-Werror=date-time//)
libRegionReplay_la_CXXFLAGS = $(LLVMCXXFLAGS)


lib_LTLIBRARIES = libRegionReplay.la
libRegionReplay_la_SOURCES = RegionReplay.cpp LoopRegionReplay.cpp OmpRegionReplay.cpp
libRegionReplay_la_LDFLAGS =
