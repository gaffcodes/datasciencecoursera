corr <- function(directory, threshold=0) {
    openfile <-function(directory, monitor) {
        if (monitor < 10) {
            c(directory, "/00", monitor, ".csv")
        }
        else if (monitor < 100) {
            c(directory, "/0", monitor, ".csv")
        }
        else {
            c(directory, "/", monitor, ".csv")
        }
    }
    
    source("complete.R")
    observations <- complete(directory, 1:332)
#    print(observations)
#    print("monitors:")
    monitors <- observations$id[observations$nobs > threshold]
#    print(monitors)

    correlations <- c()
    for(monitor in monitors) {
        filename <- paste(openfile(directory, monitor), collapse="")
        monitorData <- read.csv(filename)
        print(head(monitorData))
        correlations <- c(correlations, cor(monitorData$sulfate, monitorData$nitrate, use="complete.obs"))
    }
    if (is.null(correlations)) {
        correlations = vector("numeric", 0)
    }
    correlations
}