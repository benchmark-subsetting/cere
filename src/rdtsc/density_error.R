#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(require(ggplot2, quietly=TRUE))
options(warn=-1)
options(error=traceback)

errorFile = args[1]

d = read.table(errorFile,
               col.names=c("CodeletName", "error", "part"),
               fill=FALSE,
               strip.white=TRUE)


ggplot(d, aes(x=error, y=part)) + geom_bar(stat = "identity")
