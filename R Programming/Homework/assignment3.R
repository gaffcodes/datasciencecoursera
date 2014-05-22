#makeCacheMatrix takes as input a matrix, and returns a list containing four functions
#which get and set the matrix and its inverse. This function essentially caches the matrix
#and its inverse. It works by setting a number of variables to be functions which return
#or set the value of the matrix or its inverse, and then saving those functions to a list,
#which it returns.
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

#cacheSolve takes as input a cachedmatrix, i.e., the result of makeCacheMatrix, updates it with the inverse,
#and returns the inverse of a matrix. An example of using the two functions would be:
#inverse <- cacheSolve(makeCacheMatrix(test))

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
