## makeCacheMatrix creates a special "matrix" containing a list of functions
## to get/set the value of target matrix and cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	cachedInverse <- NULL
	set <- function(y) {
		x <<- y
		cachedInverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) cachedInverse <<- inverse
	getInverse <- function() cachedInverse
	list(
		set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse
	)
}

## cacheSolve calculates the inverse matrix of the special "matrix" created by makeCacheMatrix function
## It retrieves the cached inverse matrix if already calculated previously
cacheSolve <- function(x, ...) {
        cachedInverse <- x$getInverse()
	if (!is.null(cachedInverse)) {
		message("getting cached data")
		return(cachedInverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setInverse(inverse)
	inverse
}

## Example usage:
## Variable assignment
## x <- matrix(c(-1, 1, 1.5, -1), nrow=2, ncol=2)
## y <- makeCacheMatrix(x)
##
## First call will trigger actual computation
## Second call onwards will retrieve result from cache (message "getting cached data" should appear)
## inverse <- cacheSolve(y)
##
## Verify result is actually the inverse of original matrix (should return identity matrix)
## x %*% inverse
