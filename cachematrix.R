
## makeCacheMatrix creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## x is a square invertible matrix
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
        ## use <<- to assign a value to an object in an environment
        ## different from the current
    }
    get = function() x
    setinv = function(inverse) inv <<- inverse 
    getinv = function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
    
    ## return: a list containing functions to
    ##              1- set the matrix
    ##              2- get the matrix
    ##              3- set the inverse
    ##              4- get the inverse
}


## cacheSolve calculates the inverse of the matrix above, 
## if the inverse has been previously calculated then cacheSolve
## should retrieve the inverse from the cache to save time and
## memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv = x$getinv()
    
    # if the inverse has already been calculated
    if (!is.null(inv)){
        # then retrieve from the cache. 
        return(inv)
    }
    
    # else, calculate the inverse 
    mat.data = x$get()
    inv = solve(mat.data, ...)
    
    # place the new inverse matrix in the cache using setinv
    x$setinv(inv)
    
    return(inv)
    }
