## makeCacheMatrix takes as input a matrix and returns a list of functions that
## retrieve the matrix and its inverse and allow one to reset them.
##    cacheSolve checks a "matrix" of the above type and calculates and sets its
## inverse only if said inverse has not already been calculated.


## Contains 4 functions get, set, setInverse and getInverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(p) {
            x <<- p
            inv <<- NULL
      }
      get <- function()
            x
      setInverse <- function(inverse)
            inv <<- inverse
      getInverse <- function()
            inv
      list(
            set = set,
            get = get,
            setInverse = setInverse,
            getInverse = getInverse
      )
      
      
}


## cacheSolve checks if a matrix is square, then checks if the mean hasn't
## already been calculated and sets it.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      matrix <- x$get()
      
      if(NROW(matrix) != NCOL(matrix)){         ##Checks if matrix is square
            print("Matrix is not square")       ## and sets inverse to NULL if not
            x$setInverse(NULL)
      }
      else{
            if(!is.null(x$getInverse())){
                  print("Getting cached inverse")
                  return(x$getInverse())
            }
            inverse <- solve(matrix)
            x$setInverse(inverse)
            inverse
      }
}
