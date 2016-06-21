LLVMCXXFLAGS    := $(shell "$(LLVM_CONFIG)" --cxxflags | sed s/-Wcovered-switch-default// | sed s/-Werror=date-time//)
libcere_globalrename_la_CXXFLAGS = $(LLVMCXXFLAGS)


lib_LTLIBRARIES = libcere_globalrename.la
libcere_globalrename_la_SOURCES = GlobalRename.cpp
libcere_globalrename_la_LDFLAGS =
