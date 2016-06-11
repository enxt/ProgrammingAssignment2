## Function to make the solve of a matrix and cache it
## and the function to use the cached value

## Function to cache the solve the last matrix setted
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	get <- function() x
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## If the makeCacheMatrix has null the value of the solve matrix
## the cacheSolve calc the solve and set int the makeCacheMatrix
## else use it and return the cached value.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}

	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
