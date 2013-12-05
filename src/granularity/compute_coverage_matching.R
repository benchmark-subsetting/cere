#!/usr/bin/env Rscript

suppressPackageStartupMessages(require(ggplot2, quietly=TRUE))
suppressPackageStartupMessages(require(plyr, quietly=TRUE))

options(warn=-1)

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

aggregateData <- function(allLoops, invitroRes) {
    aggregatedResults = data.frame(CodeletName = allLoops$Codelet.Name)
    aggregatedResults$invivoCallCount = allLoops$CallCount
    aggregatedResults$invivoCPU_CLK_UNHALTED_CORE = allLoops$CPU_CLK_UNHALTED_CORE
    aggregatedResults$invitroCallCount = invitroRes[invitroRes$Codelet.Name %in% aggregatedResults$CodeletName, ]$CallCount
    aggregatedResults$invitroCPU_CLK_UNHALTED_CORE = invitroRes[invitroRes$Codelet.Name %in% aggregatedResults$CodeletName, ]$CPU_CLK_UNHALTED_CORE
    return(aggregatedResults)
}

#count the number of loops with less then 15% difference
#between invivo and invitro sycles measure
compute_matching <- function(aggregatedResults) {
    res <- ddply(aggregatedResults, c("CodeletName"), function(x) {
            data.frame(val = abs(x$invivoCPU_CLK_UNHALTED_CORE/x$invivoCallCount - x$invitroCPU_CLK_UNHALTED_CORE/x$invitroCallCount)/pmax(x$invivoCPU_CLK_UNHALTED_CORE/x$invivoCallCount, x$invitroCPU_CLK_UNHALTED_CORE/x$invitroCallCount))
    }
    )
    return(data.frame(CodeletName = res[res$val < 0.15, ]$CodeletName))
#~     return(nrow(res[res$val < 0.15, ])/nrow(aggregatedResults))
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

benchName = args[1]

loopsToKeep = read.table(paste(benchName, "/loop_to_keep", sep=""),
               comment.char = "",
               col.names=c("CodeletName"),
               strip.white=TRUE)

#Open in-vivo measure
allLoops = load_csv(paste(benchName, "/all_loops.csv", sep=""))
allLoops = allLoops[allLoops$Codelet.Name %in% loopsToKeep$CodeletName, ] #keep only loop to keep
allLoops = remove_prefix(allLoops, "__invivo__") #remove the prefix
allLoops <- allLoops[sort(allLoops$Codelet.Name),]
#~ print(allLoops)

#Open invitro measures
invitroRes = load_csv(paste(benchName, "/results/invitro_results.csv", sep=""))
invitroRes = remove_prefix(invitroRes, "__extracted__") #remove prefix
invitroRes = invitroRes[invitroRes$Codelet.Name %in% allLoops$Codelet.Name, ] ##keep only loop to keep
invitroRes <- invitroRes[order(match(invitroRes[,"Codelet.Name"], allLoops[,"Codelet.Name"])), ] #order as the invivo data
#~ print(invitroRes)

aggregatedResults = aggregateData(allLoops, invitroRes) #merge invivo and invitro
#~ print(aggregatedResults)

appCycles = load_csv(paste(benchName, "/app_cycles.csv", sep=""))
results = data.frame(BenchName = benchName)
matchingCodelet = compute_matching(aggregatedResults)
results$matching = nrow(matchingCodelet)/nrow(aggregatedResults)
results$PerrAppMatching = sum(allLoops[allLoops$Codelet.Name %in% matchingCodelet$CodeletName, ]$CPU_CLK_UNHALTED_CORE)/as.numeric(appCycles$CPU_CLK_UNHALTED_CORE)
results$coverage = sum(allLoops$CPU_CLK_UNHALTED_CORE)/as.numeric(appCycles$CPU_CLK_UNHALTED_CORE)

print(paste("Bench: ", results$BenchName, sep=""))
print(paste("Coverage = ", results$coverage*100, "%", sep=""))
print(paste("Nb codelets Matching = ", results$matching*100, "%", sep=""))
print(paste("Perr app Matching = ", results$PerrAppMatching*100, "%", sep=""))
