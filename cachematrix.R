##
## The functions herein illustrate the use of caching to reduce computational demands
## when repeat actions on the same content are likely to occur.
## In particular, we are focusing on matrix inversion.
##
## For example, we will use the replicate() and rnorm() functions to create a square,
## invertible matrix called rm.
##
## rm <- replicate(10, rnorm(10))
##
## We will then use rm to instantiate a "cache matrix" called cm.
##
## cm <- makeCacheMatrix(rm)
##
## We will then call cacheSolve(cm) to solve for its inverse, and then use
## getinverse to return the inverse of the matrix when needed.
##
## cacheSolve(cm)
##
## cm$getinverse()
##

##
## This function create and returns a special matrix object capable
## of storing its inverse for later retrieval and use.
##
makeCacheMatrix <- function(x = matrix()) {
	mi <- NULL
	set <- function(y) {
		x <<- y
		mi <<- NULL
	}
	get <- function() x
	setinverse <- function(value) mi <<- value
	getinverse <- function() mi
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##
## This function computes the inverse of the special matrix return
## by makeCacheMatrix.  If the inverse has already been calculated (and
## the matrix has not changed) cacheSolve will retrieve and return the
## cached inverse.
##
cacheSolve <- function(x, ...) {
	mi <- x$getinverse()
	if (!is.null(mi)) {
		message("Fetching cached inverse")
		return(mi)
	}
	data <- x$get()
	mi <- solve(data)
	x$setinverse(mi)
	mi
}
