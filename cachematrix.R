## makeCacheMatrix and cacheSolve are utilized to 
## cache a value for the inverse of a matrix

## makeCacheMatrix creates a special vector which contains
## the functions to set the vector value, get the vector value
## set the value of the matrix inverse, and get the value
## of the matrix invers

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinvers = getinverse)
}


## cacheSolve consumes the vector from makeCacheMatrix.  If the inverse 
## of the matrix exists in cache, it will be retrieved.  If the value 
## does not exist, the inverse matrix will be calculated and the 
## value will be stored in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(x)
        x$setinverse(i)
        i
}
