## Matrix inversion is a costly computation and therefore, there maybe some benfit
## in caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to create a special object that stores a special
## matrix and caches its inverse.

## NOTE: We assume that the matrix supplied is always invertible.


## The following function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  
  set<-function(mat)
  {
    x<<-mat
    inv<-NULL
  }
  
  get<-function() x
  
  setinverse<-function(solve){inv<<-solve} ## 'solve' function evaluates the
                                           ##  inverse of a square matrix.
  
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by
##  makeCacheMatrix above. If the inverse has already been calculated (and the 
##  matrix has not changed), then the cacheSolve should retrieve the inverse from the
##  cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  mat <- x$getinverse()
  
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}
