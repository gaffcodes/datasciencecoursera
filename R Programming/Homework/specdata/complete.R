complete <- function(directory, id = 1:332) {
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
    
    observations <- data.frame(id, nobs = vector("numeric", length(id)))
    for(monitor in id) {
        filename <- paste(openfile(directory, monitor), collapse="")
        monitorData <- read.csv(filename)
        observations[observations$id==monitor, ] <- c(monitor, sum(complete.cases(monitorData)))
    }
    observations
}