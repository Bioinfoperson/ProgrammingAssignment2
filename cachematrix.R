## Pass in a square matrix to store it as a cached matrix object.
## Assign a new variable to this. y <- makeCacheMatrix(x)
## y$get() retrieves the original matrix
## y$set(newmatrix) gives y a new matrix
## y$setinverse(inverse) is used to assign the inverse
## y$getinverse() retrieves the inverse if it was assigned

makeCacheMatrix <- function(x = matrix(numeric(0), 0,0)) {
	i <- NULL
	set <- function(y=matrix(numeric(0), 0,0)) {
                x <<- y
                i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Assigns and caches every value in the cached matrix
## Returns the cached inverse of the matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        x$setinverse(solve(x$get()))
        i <- x$getinverse()
	return(i)
}
