## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL #holds the inverse of matrix x
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x #get the matrix
  setinv <- function(inv) invx <<- inv #set the inverse value
  getinv <- function() invx #get the inverse value
  list( #access the properties of the matrix
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv
  )
}


## Write a short comment describing this function

cacheSolve <- function(x,...) {
  ## Return a matrix that is the inverse of 'x'
  invx <- x$getinv() #get the cached value
  if (!is.null(invx)) { #if it's already cached, return the value
    message("getting cached data")
    return(invx)
  }
  data <- x$get() #if it's not cached yet, then get the matrix
  invx <- solve(data, ...) #invert the matrix
  x$setinv(invx) #store the inverted value in the cache
  invx
}
