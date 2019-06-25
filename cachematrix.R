## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a function which creates a special "matrix" object cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  
  # 'makeCacheMatrix' 
  # makes a cache matrix from a given matrix
  #---------------------------------------------------------
  # 1. initialize the cache Matrix 'cacheMatrix'
  
  
  inv <- NULL # assign the value NULL for the first initialization
  # 2. define the method named 'set'
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # 3. define the method named 'get'
  # return the matrix 'x'
  get <- function() x
  # 4. define the method named 'setinv'
  setinv <- function(inverse) inv <<- inverse
  # 5. define the method named 'getCache'
  # that will return the cached inverse of 'x'
  getinv <- function() inv
  # 6. list the names of all methods that will be known to the outside world
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Write a short comment describing this function
## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  
  # 'cacheSolve'
  # return the inverse of a given matrix utilizing the cache
  #---------------------------------------------------------
  # 1. check the content of cache matrix
  inv <- x$getinv()
  # 2. if the content is not null then: return the result 
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  
  # get the matrix, create, set, update and return the cache matrix
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


