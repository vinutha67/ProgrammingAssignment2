## Matrix inversion is a costly operration and may benefit by caching the inverse of the matrix.
##Given an invertible square matrix, the below function pair cache the inverse of the 
## matrix. 

## Function makeCacheMatrix: This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      mat_dim <- dim(x)
 
      if( mat_dim[1] != mat_dim[2]) {
          print("not a square invertible matrix")
          return(x)
      }
      m_inv <- NULL
  
 
      get <- function() x
  
  
      setinverse <- function(temp_inverse)  m_inv <<- temp_inverse
      getinverse<- function() m_inv
  
      list(get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the "matrix" returned by 
## makeCacheMatrix function. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_c <- x$getinverse()
        if(!is.null(m_c)) {
            message("getting cached data")
            return(m_c)
        }
  
        data <- x$get()
    
        ## Inverse of a square matrix can be computed with the solve function in R.
        m_c <- solve(data)
        x$setinverse(m_c)
        m_c
}
