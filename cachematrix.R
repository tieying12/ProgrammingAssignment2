## The following two functions makeCacheMatrix and cacheSolve are for caching a matrix 
## together with its inverse. If in a computation this inverse is needed once again
## there is an immediate access to this inverse so that a recomputation is avoided.



## For matrix x makeCacheMatrix generates a "special" matrix object for caching x and its inverse.
## Input: an ivertible matrix x
## Launching 'makeCacheMatrix(x)' yields a list of four functions set, get, setInverse and getInverse
## set for setting the matrix you want to cache
## get for getting the cached matrix
## setInverse for setting the inverse matrix you want to cache (used in the definition of cacheSolve)
## getInverse for getting the cached inverse

makeCacheMatrix <- function(x = matrix()) {
      mat <- NULL
      set <- function(y) {
            x <<- y
            mat <<- NULL
      }
      get <- function() x
      setInverse <- function(inverseMat) mat <<- inverseMat
      getInverse <- function() mat
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}




## cacheSolve computes for a cached special matrix its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
      if(!is.list(x)) {                           ## x  is a non special matrix
            message("Error! You are trying to compute the inverse of a non-special matrix")
      } else {
            m <- x$getInverse()
            if(!is.null(m)) {          ## the inverse has already been computed
                  message("getting cached inverse matrix")
                  return(m)
            }
            matr <- x$get()             ## matr is the matrix which is cashed in the special matrix x
            m <- solve(matr, ...)       ## m is the inverse of matr
            x$setInverse(m)             ## caches the inverse m in the special matrix x
            m
      }
}
