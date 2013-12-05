#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(ggplot2, quietly=TRUE))
suppressPackageStartupMessages(require(plyr, quietly=TRUE))

options(warn=-1)

TOLERANCE=0.15

# load_csv: reads a csv file
load_csv <- function(file) {
    return (tryCatch(
                read.csv(file, header=T),
                error = function(e) {
                        print(paste("Could not open file", file))
                        q()
                    }
                ))
}

remove_prefix <- function(toChange, prefix) {
    toChange <- ddply(toChange, c("Codelet.Name"), function(x) {
                data.frame(Codelet.Name = tail(unlist(strsplit(toString(x$Codelet.Name), split=prefix, fixed=T)), n=1),
                          CallCount = x$Call.Count,
                          CPU_CLK_UNHALTED_CORE = x$CPU_CLK_UNHALTED_CORE)
                }
    )
    return(toChange)
}

dump_to_file <- function(matchingCodelet) {
    write.table(matchingCodelet$CodeletName, "matching_codelets", sep="", quote = F, row.names = F, col.names = F)
}

aggregateData <- function(allLoops, invitroRes) {
    aggregatedResults = merge(allLoops, invitroRes, by="Codelet.Name", suffixes=c("_invivo",
                                                                                  "_invitro"))
    return(aggregatedResults)
}

#count the number of loops with less then 15% difference
#between invivo and invitro sycles measure
compute_matching <- function(aggregatedResults) {
    res <- ddply(aggregatedResults, c("Codelet.Name"), function(x) {
            data.frame(val = abs(x$CPU_CLK_UNHALTED_CORE_invivo/x$CallCount_invivo - x$CPU_CLK_UNHALTED_CORE_invitro/x$CallCount_invitro)/pmax(x$CPU_CLK_UNHALTED_CORE_invivo/x$CallCount_invivo, x$CPU_CLK_UNHALTED_CORE_invitro/x$CallCount_invitro))
    }
    )
    return(data.frame(CodeletName = res[res$val < TOLERANCE, ]$Codelet.Name))
}

plot_results <- function(results) {
    p <- ggplot(results)
    p <- p + geom_bar(aes(x="matching", y=matching))
    p <- p + geom_bar(aes(x="coverage", y=coverage))
#~     p <- p + geom_point(aes(y=ref_vivo_CPInv/S$ref_clk, 
#~                               col="R", shape="R"), size=1.5)
#~     p <- p + geom_point(aes(y=tar_vivo_CPInv/S$tar_clk, 
#~                               col="T2", shape="T2"), size=1.5)
#~     p <- p + geom_point(aes(y=Predicted_cyclesPerIteration/S$tar_clk, 
#~                               col="T1", shape="T1"), size=1.5)
#~     p <- p + scale_y_log10(breaks=c(1,10,50,100,200,400,800,1500,3000,5000,10000,20000))
    p <- p + labs(y = "Percentage", x=NULL)
#~     p <- p + scale_x_discrete(breaks = NA)
#~     p <- p + theme_bw(base_size=9)
    print(p)
}

args <- commandArgs(trailingOnly = TRUE)

# Check the format of the script
if (length(args)!=1) {
    print("usage: ./plot-coverage-matching.R <Path to bench>")
    q()
}

benchDir = args[1]

#Open in-vivo measure
allLoops = load_csv(paste(benchDir, "/all_loops.csv", sep=""))
allLoops = remove_prefix(allLoops, "__invivo__") #remove the prefix

#Open invitro measures
invitroRes = load_csv(paste(benchDir, "/results/invitro_results.csv", sep=""))
invitroRes = remove_prefix(invitroRes, "__extracted__") #remove prefix
aggregatedResults = aggregateData(allLoops, invitroRes) #merge invivo and invitro

results = data.frame(BenchName = benchDir)
matchingCodelet = compute_matching(aggregatedResults)
dump_to_file(matchingCodelet)

