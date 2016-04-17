## makeCacheMatrix creates a special "matrix" (really a list contaning a number of functions and a matrix)
## that can be used with the CacheSolve function. 
## The cacheSolve function is used to calculate the inverse of the matrix in the vector made using the makeCacheMatrix funtion
## if the inverse of the matrix has already been solved, it retreived and return the cached inverse

##makeCacheMatrix function in oder to create the special list
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve funtion to calculate the inverse of the vector made using the makeCahceMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
