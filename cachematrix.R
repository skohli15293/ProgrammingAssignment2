## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix())
{
    i <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(x = x, set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
    i <- x$getinverse()
    data <- x$get()
    if(identical(x$x,data))
    {
        if(!is.null(i)) {
            message("getting cached data after checking matrix has not changed")
            return(i)
        }
    }
    i <- solve(data)
    x$setinverse(i)
    ## Return a matrix that is the inverse of 'x'
    i
}

