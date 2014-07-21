## Functions that "memoize" the calculation of the inverse matrix
## operation by creating an object that holds both the matrix and
## its inverse.

## This function constructs an object that holds the matrix and
## the cached value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
	    setinverse = setinverse,
	    getinverse = getinverse)
}

## Function that checks if we have already computed and cached
## the inverse of the matrix before (possibly expensively)
## calculating it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	## Check if cache already has the inverse
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
