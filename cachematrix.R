##  makeCacheMatrix takes a matrix, caches it, and creates 4 functions to manipulate
##  the cached value.
##  cacheSolve uses the values in the stored matrix and finds the inverse.
##  If the already inverse exists, the cached value is returned instead.

##  makeCacheMatrix takes a square matrix that has an inverse as an input
##  and creates an object. The cached matrix contains 4 functions:
## [1] set
## [2] get
## [3] getInverse
## [4] setInverse
## Author: Chris Calvin
## Date: 11-23-14

makeCacheMatrix <- function(x = matrix()){  ## x is a square matrix that has an inverse
    i <- NULL                                ## inverse is set to NULL
    
    set <- function(y){                      ## allows user to change the matrix
        x <<- y                              ## and resets the inverse to NULL
        i <<- NULL
    }
    get <- function() x                      ## returns the matrix to find the inverse
    getInverse <- function() i               ## returns the inverse
    setInverse <- function(inverse) i<<-inverse  ## sets the inverse
    list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


##  cacheSolve takes a square matrix that has an inverse as an input and
##  returns an inverse matrix. 
##  Author: Chris Calvin
##  Date: 11-23-14

cacheSolve <- function(x, ...){         ## x is a matrix that has an inverse
    i = x$getInverse()                  ## gets the inverse of 
    if(!is.null(i)) {                   ## returns inverse if it is cached
        message("getting cached data")
        return(i)
    }
    data <- x$get()                     ## loads the matrix into 'data'
    i <- solve(data,...)                ## finds the inverse of the matrix
    x$setInverse(i)                     ## caches the inverse
    i
}