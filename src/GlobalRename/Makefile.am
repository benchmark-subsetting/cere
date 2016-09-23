LLVMCXXFLAGS    := $(shell "$(LLVM_CONFIG)" --cxxflags | sed s/-Wcovered-switch-default// | sed s/-Werror=date-time//)
libGlobalRename_la_CXXFLAGS = $(LLVMCXXFLAGS)


lib_LTLIBRARIES = libGlobalRename.la
libGlobalRename_la_SOURCES = GlobalRename.cpp
libGlobalRename_la_LDFLAGS =
