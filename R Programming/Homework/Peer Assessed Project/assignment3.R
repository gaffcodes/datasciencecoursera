makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<-y
        i <<-NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...) #pass the extra arguments given to cacheSolve() through to solve()
    x$setinverse(inverse)
    inverse
}