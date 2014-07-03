#!/usr/bin/env Rscript 

args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(require(ggplot2, quietly=TRUE))
suppressPackageStartupMessages(require(reshape2, quietly=TRUE))
suppressPackageStartupMessages(require(data.table, quietly=TRUE))
suppressPackageStartupMessages(require(plyr, quietly=TRUE))
suppressPackageStartupMessages(require(GMD, quietly=TRUE))
suppressPackageStartupMessages(require(fpc, quietly=TRUE))
suppressPackageStartupMessages(require(cluster, quietly=TRUE))
options(warn=-1)
options(error=traceback)
MAX_POINTS=50000
ROOT_PLOTS = "measures/plots/"
set.seed(2000)

# Check the arguments 
if ((length(args) < 3)) {
    print("usage: ./plot_codelets.R <codelet_name> <Nb binary files> <binary files list>")
    print("If called without specifying the number of cluster, the scrpit will try to find the best number")
    q()
}

plot_by_Codelets <- function(tracedLoop, CodeletName) {
    p <- ggplot(tracedLoop, aes(y=values, x=invocation))
    p <- p + geom_point()
    p <- p + ggtitle(CodeletName)
    ggsave(file="test.png", plot=p, dpi=300)
}

plot_by_Cluster <- function(table, CodeletName) {
    p <- ggplot(table, aes(y=values, x=invocation, colour=as.character(Cluster), shape = is.representative))
    p <- p + geom_point()
    p <- p + ggtitle(CodeletName)
    p <- p + theme_bw()
    p <- p + guides(colour=guide_legend(title="Cluster"))
    ggsave(file= paste(ROOT_PLOTS,CodeletName,"_byPhase.png",sep=""), plot=p, dpi=300)
}

# load_csv: reads a csv file
load_csv <- function(csvFile) {
    return (tryCatch(
                read.table(csvFile, header=T, comment.char = "", sep=","),
                error = function(e) {
                        print(paste("Could not open file", csvFile))
                        q()
                    }
                ))
}

dump_to_file <- function(todump, filename) {
    write.table(todump, filename, sep=" ", quote = F, row.names = F, col.names = F)
}

plot_withinss <- function(cycles) {
    wss <- numeric(15)
    for (i in 1:15) wss[i] <- sum(kmeans(cycles$values,
                                       centers=i)$withinss)
    return(wss)
}

clust.centroid = function(i, dat, clusters) {
  ind = (clusters == i)
  return(median(dat[ind, drop=FALSE]))
}

clust.sum = function(i, dat, clusters) {
  ind = (clusters == i)
  return(sum(as.numeric(dat[ind, drop=FALSE])))
}

clust.length = function(i, dat, clusters) {
  ind = (clusters == i)
  return(length(dat[ind, drop=FALSE]))
}

cluster <- function (cycles) {
    if(nrow(cycles) <= 10)
    {
        nb_clust=nrow(cycles)
        print(paste("Nb Clusters = ", nb_clust, sep=""))
        return(seq(1:nb_clust))
    } else {
        pamk.best <- pamk(cycles$values, usepam=FALSE, scaling=TRUE, diss=FALSE)
        nb_clust=pamk.best$nc
        print(paste("Nb Clusters = ", nb_clust, sep=""))
        clusters = clara(cycles$values, nb_clust, metric = "ch", samples=50)
        return(clusters$clustering)
    }
}

CodeletName = args[1]
nbLoopFiles = as.integer(args[2])
print(CodeletName)
allLoops = load_csv(args[3])

nbValues=allLoops[allLoops$Codelet.Name==CodeletName, ]$Call.Count
allValues <- matrix(ncol=nbLoopFiles, nrow=nbValues)
for(i in 1:nbLoopFiles) {
    binFile=args[3+i]
    to.read=file(binFile, "rb")
    tracedLoop=readBin(to.read, double(), n=nbValues*2)
    values <- tracedLoop[seq(1,nbValues*2, 2)]
    allValues[,i] <- values
}

allValues <- data.frame(allValues)
names(allValues) <- paste("repetition_", seq(1:nbLoopFiles), sep='')
allValues$invocation=seq(1,nbValues)
total_invocation = nrow(allValues)
total_cycles = sum(as.numeric(allValues[,paste("repetition_", nbLoopFiles, sep='')]), na.rm=T)
print(paste("Trace Cycles =", total_cycles))
print(paste("Invocations=", total_invocation))
if (total_cycles - allLoops[allLoops$Codelet.Name==CodeletName, ]$CPU_CLK_UNHALTED_CORE)
{
    print(paste("Sanity check failed: Trace cycles =", total_cycles, "& cycles =", allLoops[allLoops$Codelet.Name==CodeletName, ]$CPU_CLK_UNHALTED_CORE))
}

POINTS_TO_KEEP=min(MAX_POINTS, total_invocation)

if (total_invocation > MAX_POINTS) {
    allValues=allValues[sample(nrow(allValues), POINTS_TO_KEEP, replace=F), ]
} 

if (nbLoopFiles > 1)
{
    cycles=data.frame(values=apply(allValues[,c(paste("repetition_", seq(1:nbLoopFiles), sep=''))],1,median), invocation=allValues$invocation)
} else {
    cycles=data.frame(values=allValues$repetition_1, invocation=allValues$invocation)
}

total_kept_cycles = sum(cycles$values)
print(total_kept_cycles)



#Clusterize data
clusters = cluster(cycles)
cycles = cbind(cycles, Cluster=clusters)

#Compute centroids of each cluster
Centroids <- sapply(sort(unique(cycles$Cluster)), clust.centroid, cycles$values, cycles$Cluster)
#Compute for each cluster its size
Sums <- sapply(sort(unique(cycles$Cluster)), clust.sum, cycles$values, cycles$Cluster)

#Compute for each invocation, the distance to its cluster centroid.
#And elect as representative invocation, the closest to the centroid.
cycles$is.representative=F
for (clust in unique(cycles$Cluster)) {
    tmp = cycles[cycles$Cluster==clust, ]
    tmp$Cluster <- NULL
    center = Centroids[[clust]]

    tmp$DistToCentroid = abs(tmp$values - center)
    tmp <- tmp[order(tmp$DistToCentroid), ] #order first by distance to centroid
    #We select as representative the closest invocation to the centroid
    cycles[cycles$Cluster==clust, ]$is.representative =
      ifelse(cycles[cycles$Cluster==clust, ]$invocation == tmp[1, ]$invocation, T, F)
}

plot_by_Cluster(cycles, CodeletName)

res <- cycles[cycles$is.representative==T, ]
res <- res[order(res$Cluster), ]
#The part of each cluster time in the total loop time.

res$part = (Sums/res$values) * total_cycles/total_kept_cycles
#best=sum(res$values*res$part)*total_invocation
#best_error=abs(best-total_cycles)/pmax(best, total_cycles)
print(res)
#print(paste("Best error is", best_error))
todump=data.frame(res$invocation, res$part, res$values)
dump_to_file(todump, paste(CodeletName, ".invocations", sep=""))
