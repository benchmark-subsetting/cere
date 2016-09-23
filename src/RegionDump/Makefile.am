LLVMCXXFLAGS    := $(shell "$(LLVM_CONFIG)" --cxxflags | sed s/-Wcovered-switch-default// | sed s/-Werror=date-time//)
libRegionDump_la_CXXFLAGS = $(LLVMCXXFLAGS)


lib_LTLIBRARIES = libRegionDump.la
libRegionDump_la_SOURCES = RegionDump.cpp LoopRegionDump.cpp OmpRegionDump.cpp
libRegionDump_la_LDFLAGS =
