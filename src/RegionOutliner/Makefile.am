LLVMCXXFLAGS    := $(shell "$(LLVM_CONFIG)" --cxxflags | sed s/-Wcovered-switch-default// | sed s/-Werror=date-time//)
libRegionOutliner_la_CXXFLAGS = $(LLVMCXXFLAGS)


lib_LTLIBRARIES = libRegionOutliner.la
libRegionOutliner_la_SOURCES = RegionExtractor.cpp LoopRegionOutliner.cpp OmpRegionOutliner.cpp
libRegionOutliner_la_LDFLAGS =
