addnums <- function(a,b) {
  a+b
}

above10 <- function(x) {
  use <- x > 10 #logical vector
  x[use]
}

above <- function(x, n) {
  use <- x>n
  x[use]
}

columnmean <- function(y, removeNA = TRUE) {
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[, i], na.rm = removeNA)
  }
  means   #return means
}