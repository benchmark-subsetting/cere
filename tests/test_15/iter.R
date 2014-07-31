#!/usr/bin/env Rscript 

args <- commandArgs(trailingOnly = TRUE)

options(warn=-1)
options(error=traceback)

CodeletName = args[1]
invocation = as.integer(args[2])
binFile = paste(CodeletName, ".bin", sep="")

nbValues=invocation
to.read=file(binFile, "rb")
tracedLoop=readBin(to.read, double(), n=nbValues*2)
cat((tracedLoop[seq(1,nbValues*2, 2)])[invocation], "\n")
