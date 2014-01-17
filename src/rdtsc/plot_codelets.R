#!/usr/bin/env Rscript 

args <- commandArgs(trailingOnly = TRUE)
suppressPackageStartupMessages(require(ggplot2, quietly=TRUE))
suppressPackageStartupMessages(require(reshape2, quietly=TRUE))
suppressPackageStartupMessages(require(data.table, quietly=TRUE))

plot_by_Codelets <- function(allLoops) {
    meltedLoops<-melt(allLoops, id.vars=c("Codelet.Name","Call.Count","CPU_CLK_UNHALTED_CORE"))
    meltedLoops<-meltedLoops[!(is.na(meltedLoops$value)), ]
    p <- ggplot(meltedLoops, aes(x=variable, y=value))
    p <- p + geom_point()
    p <- p + facet_wrap(~Codelet.Name, scales="free")
    print(p)
}

# Check the arguments 
if ((length(args) != 1) && (length(args) != 2)) {
    print("usage: ./plot_codelets.R <rdtsc_file.csv> [<Codelet Name>]")
    print("    Optional value: Codelet Name")
    q()
}

# load_csv: reads a csv file
load_csv <- function(file) {
    return (tryCatch(
                read.csv(file, header=F, fill=T, sep=",", col.names=c("Codelet Name", "Call Count", "CPU_CLK_UNHALTED_CORE", paste("V", seq_len(max(count.fields(file, sep=",", comment.char=""))-3)))),
#~                 fread(file, sep=",", header=F, verbose=T, colClasses=c("Codelet Name", "Call Count", "CPU_CLK_UNHALTED_CORE", paste("V", seq_len(max(count.fields(file, sep=",", comment.char=""))-3)))),
                error = function(e) {
                        print(paste("Could not open file", file))
                        q()
                    }
                ))
}

rdtsc_file = args[1]
if(length(args) == 2) {
    CodeletName=args[2]
}

#Open in-vivo measure
#~ allLoops <- fread(rdtsc_file, header=F, verbose=T, autostart=100L)
allLoops = load_csv(rdtsc_file)
#~ print(head(allLoops))
allLoops=allLoops[-1,] #Remove header
if(length(args) == 2) {
    allLoops=allLoops[allLoops$Codelet.Name==CodeletName, ]
}
#~ mysample<-allLoops[sample(1:nrow(allLoops), 50, replace=FALSE),]
plot_by_Codelets(allLoops)

