## This function creates a special "matrix" object that can 
## cache its inverse

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setimatrix <- function(solve) i <<- solve
  getimatrix <- function() i
  list(set = set, get = get, setimatrix = setimatrix, 
       getimatrix = getimatrix)
}


##  This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
##  If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse 
## from the cache.n
##  If not, the function computes the inverse and sets the 
## value in the cache using the 'setimatrix" function.

cacheSolve <- function(x, ...) {
    i <- x$getimatrix()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setimatrix(i)
    i
}
