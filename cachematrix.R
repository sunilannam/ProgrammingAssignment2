## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly

## makeCacheMatrix  function creates a special "matrix" object that
## can cache its inverse.

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the invesrse of the matrix
## 4. get the value of the invesrse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y){
    x <<- y
    inv <-- NULL
  }
  get = function() { x }
  setinv = function(inverse) { inv <<- inverse}
  getinv = function() { inv }
  
  # Create list of functions returning matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  
  inv = x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
     inv
  }
  
  data <- x$get()
  inv = solve(data,...)
  x$setinv(inv)
  inv
}
