## ===========================================================================
## R Programming Course - Week 3 Programming Assignment 2: Lexical Scoping
## ===========================================================================

## The two functions below invert a matrix and cache the inverse.

# This function creates a list of getter and setter functions and two data objects. 
# It enables the function below to cache a matrix inverse.   

makeCacheMatrix <- function (x = matrix()){
        inv <- NULL                                        # initialise matrix inverse variable         
        set <- function(y){                                # reset any predetermined values of x and inv
                x <<- y
                inv <<- NULL
        } 
        get <- function () {x}
        setinv <- function (inverse) {inv <<- inverse}
        getinv <- function () {inv}
        list (get = get, set = set,
              setinv = setinv, 
              getinv = getinv)
}


# This function computes the inverse of a matrix returned by makeCacheMatrix above. 
# Assuming that the matrix is non-singular, if the inverse has already been calculated, 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function (x,...){
        inv <- x$getinv()
        if(!is.null(inv)){
                message("matrix inverse already exists, getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setinv(inv)
        inv
}