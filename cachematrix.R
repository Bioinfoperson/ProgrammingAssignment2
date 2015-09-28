## Put comments here that give an overall description of what your
## functions do

## Pass in a square matrix to store it and it's inverse in a list.
## Assign this to a name and use instead of the original matrix.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- solve(x)
	set <- function(y=matrix()) {
                x <<- y
                inverse <<- solve(x)
	}
      get <- function() x
      getinverse <- function() inverse
      list(set = set, get = get, getinverse = getinverse)
}


## Replace the inverse function with this.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        m <- x$set(x)
        m
}

