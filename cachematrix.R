## Two functions that are used to create a special matrix object,
## compute its inverse, cache the result and return it.

## Function makeCacheMatrix takes a matrix object argument, 
## defines getter and setter functions and returns them in a 
## list.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverted <- function(inverted) inv <<- inverted
    getInverted <- function() inv
    list(set = set, get = get,
         setInverted = setInverted,
         getInverted = getInverted)
}


## Function cacheSolve takes the list object returned by 
## makeCacheMatrix, returns the inversed matrix if it is  
## cached, computes the inversed matrix if it isn't cached,
## sets it in cache and returns the computeted inverse.
## Error is thrown if matrix is invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverted()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data = x$get()
    inv <- solve(data)
    x$setInverted(inv)
    inv
}
