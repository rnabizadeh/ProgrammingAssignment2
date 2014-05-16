## The Two functions are used to cach matrix. makeCacheMatrix is for creating the methods and 
## cacheSolve is used tio inverse a matrix

## This function is for creating methods for Caching a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmat <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmat <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(setmat = setmat, getmat = getmat,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function is for making a inverse matrix 

cacheSolve <- function(x, ...) {

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getmat()
        m <- ginv(data)
        x$setinverse(m)
        m

        
          ## Return a matrix that is the inverse of 'x'
}
