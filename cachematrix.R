## makeCacheMatrix creates a special matrix object that makes it possible to cache the 
## matrix inverse and cacheSolve either calculates the inverse (if it is not already cached)
## or retrives the cached inverse


## This function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    # returns matrix object with functions to get get and set matrix entries and to 
    # get and set the matrix inverse 
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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated, then cachesolve retrieves its inverse 
## from the cache. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        # check if inverse is already in cache
        message("getting chached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data,...) # compute inverse of matrix
    x$setinv(inv)
    inv
}
