#!/usr/bin/env Rscript 

suppressPackageStartupMessages(require(ggplot2, quietly=TRUE))
suppressPackageStartupMessages(require(data.table, quietly=TRUE))
options(warn=-1)
options(error=traceback)
ROOT_PLOTS = "measures/plots/"
MEASURE_FILE = "measures/matching_error.csv"
SELECTED_FILE = "measures/selected_codelets"

load_csv <- function(csvFile) {
    return (tryCatch(
                read.table(csvFile, header=T, comment.char = "", sep=","),
                error = function(e) {
                        print(paste("Could not open file", csvFile))
                        q()
                    }
                ))
}

plot_graph <- function(table) {
    p <- ggplot(table, aes(y=100*Exec.Time, x=100*Error))
    p <- p + geom_line()
    p <- p + theme_bw() + ylab("Exec time Cumulated (%)") + xlab("Error max allowed (%)")
    p <- p + geom_vline(xintercept=15, size = 1.5, linetype="dotted", colour = "red")
    p <- p + geom_hline(yintercept=85, size = 1.5, linetype="dotted", colour = "blue")
    ggsave(file= paste(ROOT_PLOTS,"bench_error.png",sep=""), plot=p, height=5, width=7, dpi=300)
}


main <- function() {
    table <- load_csv(MEASURE_FILE)
    codelets <- as.vector(read.table(SELECTED_FILE, sep="\n"))
    codelets <- sapply(codelets,function(x) as.character(x))
    codelets <- sapply(table[,"Codelet.Name"], function(x,y) x %in% y, y=codelets)
    table <- table[codelets,]
    ordre <- as.vector(table[,"Error"])
    ordre <- order(ordre)
    table <- table[ordre,c("Error","Exec.Time")]
    vec <- as.vector(table[,"Exec.Time"])
    table[,"Exec.Time"] <- sapply(1:length(vec),function(x, y) {sum(y[1:x])}, y = vec)
    plot_graph(table)
}


main()
