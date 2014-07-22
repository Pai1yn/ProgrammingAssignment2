makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x ##returns value of x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m ##returns value of m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  q <- x$getinverse()
  if(!is.null(q)) {
    message("getting cached data")
    return(q)
  }
  data <- x$get()
  q <- solve(data, ...)
  x$setinverse(q)
  q
}