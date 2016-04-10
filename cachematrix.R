## Fristly I have to create a function that sets up a special vector that allows
## functions do

## This function will be used on a vector and cache the result

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL
  set < - function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = get,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
      }

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. if the invesrse has already been solved then it will extract the inverse that has
##been created

cacheSolve <- function(x, ...){
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  }
