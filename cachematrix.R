## That creates a container for inverse matrix caching
## 'm' is an object of class "matrix"
makeCacheMatrix <- function(m = matrix()){
  inverse <- NULL
  
  # setting up a new matrix
  set <- function(m_new){
    m <<- m_new
    inverse <<- NULL
  }
  
  # get original matrix
  get <- function() m
  
  # save inverse matrix - setter 
  setInverse <- function(inverse_new) inverse <<- inverse_new
  
  # get cached inverse matrix - getter
  getInverse <- function() inverse
  
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}

## Gets inverse matrix by either calculating or retrieving results from cache - depending on cache state
## 'cacheMatrix' is a list object returned from 'makeCacheMatrix' function
cacheSolve <- function(cacheMatrix, ...){
  inverse <- cacheMatrix$getInverse()
  
  if(!is.null(inverse)){
    # We have inversed matrix in the cache. Returning it
    inverse
  }
  else
  {
    # We have no inversed matix in cacehe; calculating inverse matrix and storing it in cache
    m <- cacheMatrix$get()
    inverse <- solve(m, ...)
    
    cacheMatrix$setInverse(inverse)
    
    inverse
  }
