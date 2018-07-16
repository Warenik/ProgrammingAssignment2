## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## instead of matrix we will have a list with functions
## which get and set 2 objects: matrix x and its inverse
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve gets list from makeCacheMatrix as input
## and restores inverse matrix if available otherwise it add inverse mymatrix 
## to x by setinverse that is set in input(x) 
## In fact x is not a matrix
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
