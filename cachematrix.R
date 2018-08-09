## Put comments here that give an overall description of what your
## functions do

## Function is similar to the example, tweaked with minor edits.

## Write a short comment describing this function
# First function makeCacheMatrix creates a list similar to the example, containing a function to
#     1. set the matrix
#     2. get the matrix
#     3. set the inverse of the matrix 
#     4. get the the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function
# Second function checks to see if the inverse has already been computed.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the matrix and set the inverse of the matrix
# in the cache via the setinv function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix = x$get()
    inv = solve(matrix)
    x$setinv(inv)
    inv
}
