makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  SetInverse <- function(inverse) {inv <<- inverse}
  GetInverse <- function() {inv}
  list(set = set, get = get, SetInverse = SetInverse, GetInverse = GetInverse)
}

cacheSolve <- function(x, ...){
  inv <- x$GetInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$SetInverse(inv)
  inv
}