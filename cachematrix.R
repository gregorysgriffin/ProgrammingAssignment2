## Put comments here that give an overall description of what your

## function that provides a list of functions for getting and setting a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverse matrix as null
    inverse <- NULL

    # set the matrix
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    
    # get the matrix
    get <- function() x
    
    # set the inverse matrix
    setInverse <- function(inv) inverse <<- inv
    
    # get the inverse matrix
    getInverse <- function() inverse
    
    # return the functions as a list
    list(
      set = set, 
      get = get, 
      setInverse = setInverse, 
      getInverse = getInverse
      )
}



## function that solves the inverse of an invertable matrix, caching the result
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    
    # check to see if the inverse has already been calculated and is available in the cache
    if (!is.null(inverse)) {
        message("Getting cached inverse matrix")
        return(inverse)
    }
    
    # otherwise, get the input matrix
    matrix <- x$get()
    # calculate the inverse matrix
    inverse <- solve(matrix, ...)
    # store the inverse matrix to the 'cache'
    x$setInverse(inverse)
    # output the inverse matrix
    return(inverse)

}
