## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL   # The inverse matrix is set to null by default
      set <- function(y) {    # The set function for this object is used to define the matrix
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse_input) inverse <<- inverse_input
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getinverse()
      if(!is.null(inverse)) {
            message("Existing stored inverse matrix. Now checking if it is correct")
            return(inverse)
      }
      else {
            data <- x$get()
            inverse <- solve(data)
            x$setinverse(inverse)
      }
      inverse
}
