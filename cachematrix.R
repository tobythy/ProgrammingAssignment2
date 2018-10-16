## Put comments here that give an overall description of what your
## functions do

## Create a special object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## The process intialize the inverse property
    m <- NULL
    
    ## Process to set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Get the matrix
    get <- function() {
        x
    }
    
    ## Set the inverse of the matrix
    setinverse <- function(inverse) {
        m <<- inverse
    }
    
    ## Get the inverse of the matrix
    getinverse <- function() {
        m
    }
    
    ## Return a list of the process facts
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Compute the inverse of the matrix above

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    ## Return the inverse if it's already set
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Get the matrix from the data
    data <- x$get()
    
    ## Use the matrix multiplication to get the inverse
    m <- solve(data) %*% data
    
    ## Set the inverse
    x$setinverse(m)
    
    m
}
