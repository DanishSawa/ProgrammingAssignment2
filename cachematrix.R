
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        inv <- NULL
        
        set <- function(y)
        {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setInverse <- function(inverse)
        {
                inv <<- inverse       
        }
        getInverse <- function() inv
        
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above, if it has not already
## been calculated

cacheSolve <- function(x, ...) 
{
        ## Check if inverse has already been calculate or not
        inv <- x$getInverse()
        
        ## If calculated
        if (!is.null(inv))
        {
                message("Getting cached data")
                return(inv)
        }
        
        ##If not calculated
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
