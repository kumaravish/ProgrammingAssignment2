#Function makeCacheMatrix creates a special "matrix", 
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {

    # initialize the stored Matrixinverse value to NULL
    matrixInv <- NULL
    
    #set the value of the matrix
    set <- function(y) {
        x <<- y
        matrixInv <<- NULL
    }
    #Get the value of the matrix
    get <- function() x
    
    #to set the inverse
    setinv <- function(inv_) matrixInv <<- inv_
    # to get the inverse
    getinv <- function() matrixInv
    
    # return a list of all the above functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    #check if the inverse is already cached
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #not cached get the matrix into data
    data <- x$get()
    #compute the inverse
    inv <- solve(data, ...)
    #cache the inverse
    x$setinv(inv)
    #return
    return (inv)
}

