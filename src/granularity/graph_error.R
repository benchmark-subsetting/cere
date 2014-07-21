#!/usr/bin/env Rscript 

suppressPackageStartupMessages(require(ggplot2, quietly=TRUE))
suppressPackageStartupMessages(require(data.table, quietly=TRUE))
options(warn=-1)
options(error=traceback)
ROOT_PLOTS = "measures/plots/"
ERROR_TABLE = "measures/table_error.csv"

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
    p <- ggplot(table, aes(y=Exec.Time, x=Error))
    p <- p + geom_line()
    p <- p + theme_bw() + ylab("Exec time Cumulated (%)") + xlab("Error max allowed (%)")
    p <- p + geom_vline(xintercept=15, size = 1.5, linetype="dotted", colour = "red")
    p <- p + geom_hline(yintercept=85, size = 1.5, linetype="dotted", colour = "blue")
    ggsave(file= paste(ROOT_PLOTS,"bench_error.png",sep=""), plot=p, height=5, width=7, dpi=300)
}

main <- function() {
    table <- load_csv(ERROR_TABLE)
    table[nrow(table)+1,] <- c(100,table[nrow(table),"Exec.Time"])
    table[nrow(table)+1,] <- c(0,0)
    print (table)
    plot_graph(table)
}

main()
