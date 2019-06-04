## The cachematrix script contains two functions: makeCacheMatrix and cacheSolve.
## These two functions, together, provide an efficient way to store a matrix in memory and, together with it, also store its inverse
## This can be useful when the inverse of the matrix needs to be called several times. In this way, we spare a lot of calculations. 
## This can be particularly useful for large matrices

## The makeCacheMatrix function has four main functions:
## 1) It allows to set the matrix: $set
## 2) It allows to get the matrix: $get
## 3) It allows to set the inverse of the matrix: $setinverse
## 4) It allows to get the inverse of the matrix: $getinverse

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


## The cacheSolve matrix is used to get the inverse of the matrix object created using makeCacheMatrix
## - If the inverse was not yet calculated and stored, it calculates it, and stores it in the object
## - If the inverse was already calculated and stored, it simply returns it

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
