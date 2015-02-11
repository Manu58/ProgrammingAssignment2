## These functions implement a cached matrix inversion using a closure
## A caching matrix object is created by calling makeCacheMatrix with a square 
## invertible matrix
## as argument returning a matrix object as a list of methods for retrieving
## and setting the matrix and its iverse.
## cacheSolve returns the inverse with the matrix object as argument. In case
## the cache variable "inv" is null,the function first calculates the inverse 
## using the solve routine of R and sets "inv" to the inverse. Otherwise the 
## matrix stored in "inv" is returned.
## In case the matrix is not square or not invertible ca;lling cacheSolve 
## returns an error

        
## Write a short comment describing this function
## Initializes the matrix object: Arg: x        an invertible matrix
##                              Value:  A list of functions 
##                                        set,get for the matrix
##                                        setinver, getinverse for the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y         # Assignement without defining local variables
                inv <<- NULL    
        }
        get <- function() x 
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## function to get the inverse. Arguments: x  a matrix object (list of methods)
##                                         ... arguments to be passed to solve
##                              Value:  the inverse of the matrix/
## Note : the return is a matrix not a caching matrix object
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv        
}

  