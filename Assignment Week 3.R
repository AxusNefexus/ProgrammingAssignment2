makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  
  get <- function()x
  setinverse <- function(inverse) j <<- inverse
  getinverse <- function() j
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


cacheSolve <- function(x, ...) {
  j <- x$getinverse()
  if(!is.null(j)){
    message("Getting cached data")
    return(j)
  
    }
  mat <- x$get()
  j <- solve(mat, ...)
  x$getinverse(j)
  j
  
}
