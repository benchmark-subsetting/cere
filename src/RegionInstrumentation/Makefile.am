LLVMCXXFLAGS    := $(shell "$(LLVM_CONFIG)" --cxxflags | sed s/-Wcovered-switch-default// | sed s/-Werror=date-time//)
libRegionInstrumentation_la_CXXFLAGS = $(LLVMCXXFLAGS)


lib_LTLIBRARIES = libRegionInstrumentation.la
libRegionInstrumentation_la_SOURCES = RegionInstrumentation.cpp LoopRegionInstrumentation.cpp OmpRegionInstrumentation.cpp
libRegionInstrumentation_la_LDFLAGS =
