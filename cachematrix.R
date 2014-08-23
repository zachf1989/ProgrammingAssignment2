## The following functions are used to create a matrix object
## that can hold a cached inverse and will calculate the inverse
## if the cache is empty, otherwise, it will only return the cached
## value

## This function creates a special "matrix" object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix())
    {
    i <- NULL
    
    set <- function(y)
      {
      x <<- y
      i <<- NULL
      }
    
    get <- function() x
    setsolve <- function(inverse) i <<- inverse
    getsolve <- function() i

    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
    }


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
    {
    i <- x$getsolve()
    
    if(!is.null(i))
      {
      message("getting cached data")
      return(i)
      }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i
    }