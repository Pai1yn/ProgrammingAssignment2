makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ##original set to NULL
  set <- function(y) {
    x <<- y ##takes matrix and makes it the argument of set function
    m <<- NULL ##resets m to NULL
  }
  get <- function() x
  setsolve <- function(inverse) m <<- inverse
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data") ##if inverse has been stored
    return(m)
  }
  data <- x$get() ##if inverse not stored, solve matrix
  m <- solve(data, ...)
  x$setsolve(m)
  m
}