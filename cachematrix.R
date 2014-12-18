## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##The first function, makeCacheMatrix creates a special "vector", to :
##get the value of the matrix
##set the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                                     ## Inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

## It calculates inverse of the matrix
## If inverse has already been calculates, the function takes value from cache
## otherwise it calculates the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
