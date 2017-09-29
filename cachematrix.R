## This pair of functions calculate an inverse of an invertible matrix, 
## and cache the inverse.
## So, in case of a repeated need of this inverse (of the unchanged matrix), 
## it is returned without being re-calculated.
## Because calculation time is more expensive than space in memory. 




## This function creates an R object (list) that caches (stores) a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {     # set the value of the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x      # get the value of the matrix
    setinverse <- function(solve) m <<- solve   # set the value of the inverse matrix
    getinverse <- function() m                  # get the value of the inverse matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function requires the argument that is returned by makeCacheMatrix() 
## in order to get or retrieve the inverse matrix from the cached value, 
## which is stored in the makeCacheMatrix()'s environment

cacheSolve <- function(x, ...) {
    m <- x$getinverse()     # trying to retrieve the inverse matrix
    if(!is.null(m)) {       # see if the result is NULL
        message("getting cached data")   
        return(m)           # if it is not NULL return the cached inverse matrix
    }
    data <- x$get()         # if the result is NULL, get the matrix from the input object
    m <- solve(data, ...)   # calculate the inverse
    x$setinverse(m)         # set the setinverse() function of the input object
    m                       # returns the inverse matrix
}

