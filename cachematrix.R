## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y) {
      x <<- y
      m_inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m_inv <<- inverse
    getInverse <- function() m_inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getInverse()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data)   ## matrix inverse
  x$setInverse(m_inv)
  m_inv
}
