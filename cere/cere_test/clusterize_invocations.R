#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

suppressPackageStartupMessages(require(ggplot2, quietly=TRUE))
suppressPackageStartupMessages(require(reshape2, quietly=TRUE))
suppressPackageStartupMessages(require(data.table, quietly=TRUE))
suppressPackageStartupMessages(require(plyr, quietly=TRUE))
suppressPackageStartupMessages(require(GMD, quietly=TRUE))
suppressPackageStartupMessages(require(fpc, quietly=TRUE))
suppressPackageStartupMessages(require(cluster, quietly=TRUE))
suppressPackageStartupMessages(require(fields, quietly=TRUE))
options(warn=-1)
options(error=traceback)
MAX_POINTS=50000
ROOT_PLOTS = "cere_measures/plots/"
PALETTE = c("red","blue","green","yellow","black","grey","pink","maroon","orange","purple","magenta",
            "silver","golden","brown","cyan")
set.seed(2000)

# Check the arguments
if ((length(args) < 3)) {
    print("usage: ./plot_codelets.R <codelet_name> <csv file> <bynary files>")
    q()
}

plot_by_Codelets <- function(tracedLoop, CodeletName) {
    p <- ggplot(tracedLoop, aes(y=cycles, x=invocation))
    p <- p + geom_point()
    p <- p + ggtitle(CodeletName)
    ggsave(file="test.png", plot=p, dpi=300)
}

plot_by_Cluster <- function(table, CodeletName) {
    p <- ggplot(table, aes(y=cycles, x=invocation, colour=as.character(Cluster), shape = is.representative))
    p <- p + geom_point()
    p <- p + theme_bw() + ylab("Cycles")
    p <- p + scale_colour_manual(values=PALETTE)
    p <- p + theme(legend.position="none")
    ggsave(file= paste(ROOT_PLOTS,CodeletName,"_byPhase.png",sep=""), plot=p, height=4.16, width=10, dpi=300)
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

plot_withinss <- function(allValues) {
    wss <- numeric(15)
    for (i in 1:15) wss[i] <- sum(kmeans(allValues$cycles,
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

cluster <- function (allValues) {
    #if the codelet has less than 11 invocations
    if(nrow(allValues) <= 10)
    {
        nb_clust=nrow(allValues)
        print(paste("Nb Clusters = ", nb_clust, sep=""))
        return(seq(1:nb_clust))
    } else {
        pamk.best <- pamk(allValues$cycles, usepam=FALSE, scaling=TRUE, diss=FALSE)
        nb_clust=pamk.best$nc
        print(paste("Nb Clusters = ", nb_clust, sep=""))
        clusters = clara(allValues$cycles, nb_clust, metric = "ch", samples=50)
        return(clusters$clustering)
    }
}

CodeletName = args[1]
print(CodeletName)
nonTraceCycles = load_csv(args[2])

inVivoCycles=nonTraceCycles[nonTraceCycles$Codelet.Name==CodeletName, ]$CPU_CLK_UNHALTED_CORE
total_invocation=nonTraceCycles[nonTraceCycles$Codelet.Name==CodeletName, ]$Call.Count

binFile=args[3]
to.read=file(binFile, "rb")
tracedLoop=readBin(to.read, double(), n=total_invocation*2)
cycles <- tracedLoop[seq(1,total_invocation*2, 2)]

invocation=seq(1,total_invocation)
allValues = data.frame(cycles, invocation)

print(paste("Cycles = ", inVivoCycles))
print(paste("Invocations=", total_invocation))

POINTS_TO_KEEP=min(MAX_POINTS, total_invocation)

if (total_invocation > MAX_POINTS) {
    allValues=allValues[sample(nrow(allValues), POINTS_TO_KEEP, replace=F), ]
}

total_kept_cycles = sum(allValues$cycles)

#Clusterize data
clusters = cluster(allValues)
allValues = cbind(allValues, Cluster=clusters)
#Compute centroids of each cluster
Centroids <- sapply(sort(unique(allValues$Cluster)), clust.centroid, allValues$cycles, allValues$Cluster)

ClusterInfo = data.frame(Cluster=1:length(Centroids), Centroid = Centroids)

# Merge clusters
find_merge <- function() {
    if (nrow(ClusterInfo) == 1) { return(list())}
    for (i in 1:nrow(ClusterInfo)) {
        for (j in i:nrow(ClusterInfo)) {
            if (j != i) {
                left = ClusterInfo[i,]$Centroid
                right = ClusterInfo[j,]$Centroid
                error = abs(left-right)/max(left,right)
                if (error <= 0.01) {
                    # merge left and right clusters
                    return(list(ClusterInfo[i,]$Cluster, ClusterInfo[j,]$Cluster))
                }
            }
        }
    }
    return(list())
}

while(1) {
    merged = find_merge()
    if (length(merged) == 0) break;
    left = merged[[1]]
    right = merged[[2]]
    ClusterInfo = ClusterInfo[ClusterInfo$Cluster != right,]
    allValues[allValues$Cluster==right,]$Cluster = left
}

#Compute for each invocation, the distance to its cluster centroid.
#And elect as representative invocation, the closest to the centroid.
allValues$is.representative=F
for (clust in unique(allValues$Cluster)) {
    tmp = allValues[allValues$Cluster==clust, ]
    tmp$Cluster <- NULL
    center = ClusterInfo[ClusterInfo$Cluster == clust, ]$Centroid

    tmp$DistToCentroid = abs(tmp$cycles - center)
    tmp <- tmp[order(tmp$DistToCentroid), ] #order first by distance to centroid

    #We select as representative the closest invocation to the centroid
    allValues[allValues$Cluster==clust, ]$is.representative =
      ifelse(allValues[allValues$Cluster==clust, ]$invocation == tmp[1, ]$invocation, T, F)
}

#compute distance between clusters
distances = rdist(ClusterInfo$Centroid)

#Find the nearest cluster to clust
find_nearest_cluster <- function(clust, distances) {
    tmp_vec = distances[,clust]
    tmp_vec[[clust]] = Inf
    tmp_vec <- order(tmp_vec)
    nearest_clust=tmp_vec[[1]]
    i=1
    while(nearest_clust != clust) {
        #We have to check if that cluster has a representative > 2000 cycles
        if (allValues[allValues$Cluster==clust & allValues$is.representative==T, ]$cycles >= 2000) {
            return(nearest_clust)
        }
        i=i+1
        nearest_clust = tmp_vec[[i]]
    }
    return(0)
}

#If a representative is below 2000 cycles, we search
#for the nearest cluster to fused them both.
if (nrow(ClusterInfo) != 1) {
    for (i in 1:nrow(ClusterInfo)) {
        clust=ClusterInfo[i,]$Cluster
        if (allValues[allValues$Cluster==clust & allValues$is.representative==T, ]$cycles < 2000) {
            nearest_clust = find_nearest_cluster(clust, distances)
            if(nearest_clust != 0) {
                allValues[allValues$Cluster==clust, ]$is.representative = F
                allValues[allValues$Cluster==clust, ]$Cluster = nearest_clust
            }
        }
    }
}

#Compute for each cluster its size
Sums <- sapply(sort(unique(allValues$Cluster)), clust.sum, allValues$cycles, allValues$Cluster)

plot_by_Cluster(allValues, CodeletName)

res <- allValues[allValues$is.representative==T, ]
res <- res[order(res$Cluster), ]

#The part of each cluster time in the total loop time.
res$part = (Sums/res$cycles) * inVivoCycles/total_kept_cycles
print(res)
todump=data.frame(res$invocation, res$part, res$cycles)
dump_to_file(todump, paste(CodeletName, ".invocations", sep=""))
