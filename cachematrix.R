## In order to save computing time the following functions will create a matrix and 
## its inverse in order to pull on the cached data to solve

## makeCacheMatrix creates a special matrix able to cache its inverse
## contains four functions -- set, get, setmena, getmean
## -set the value of the matrix
## -get the value of the matrix
## -set the value of inverse of the matrix
## -get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function()x
  setInverse = function(inverse) inv <<- inverse
  getInverse = function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve takes the inverse of the cachemean of the above matrix and stores it in the object inv
## This function assumes that the matrix is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getInverse()
  if(!is.null(inv)) {
    message("pulling cached data")
    return(inv)
  }
  data = x$get()
  inv = solve(data, ...)
  x$setInverse(inv)
  inv
}

## SAMPLE RESULTS
## > a <- diag(6,4)
## > cachedMatrix <- makeCacheMatrix(a)
## > cacheSolve(cachedMatrix)
## [,1]      [,2]      [,3]      [,4]
## [1,] 0.1666667 0.0000000 0.0000000 0.0000000
## [2,] 0.0000000 0.1666667 0.0000000 0.0000000
## [3,] 0.0000000 0.0000000 0.1666667 0.0000000
## [4,] 0.0000000 0.0000000 0.0000000 0.1666667
## > b <- diag(2,5)
## > cachedMatrix <- makeCacheMatrix(b)
## > cacheSolve(cachedMatrix)
## [,1] [,2] [,3] [,4] [,5]
## [1,]  0.5  0.0  0.0  0.0  0.0
## [2,]  0.0  0.5  0.0  0.0  0.0
## [3,]  0.0  0.0  0.5  0.0  0.0
## [4,]  0.0  0.0  0.0  0.5  0.0
## [5,]  0.0  0.0  0.0  0.0  0.5
