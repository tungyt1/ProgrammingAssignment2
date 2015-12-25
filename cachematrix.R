## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x #this obtains the matrix
    setInverse <- function(solve) m <<- solve #this sets the inverse matrix
    getInverse <= function() m #this gets the inverse matrix
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse) # to create list of functions
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
            m <- x$getInverse() #Return a matrix that is the inverse of 'x'
            if(!is.null(m)) {   #if there is a cache then inverse has already been calculated
                message("getting cached data") #returns message as noted for cached data
                return(m) #returns the cached information
            }
            data <- x$get() #obtains matrix from the makeCacheMatrix function
            m <- solve(data, ...) #calculates inverse of matrix
            x$setInverse(m) #stores the inverse matrix in cache using first function
            m
}
