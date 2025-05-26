## Create a square Matrix and cache its solved inverse 
## contains 4 functions:
## set Set the matrix
## get Get the matrix
## setinverse Set the inverse
## getinverse the inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inv <<- solve
	getinverse <- function() inv
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## Get the Matrix from the makeCacheMatrix and solve it if not already done so
## Get the solved Matrix from Cache otherwise

cacheSolve <- function(x, ...) {
	inv <- x$getinverse	
	## Return Cache if available
	if(!is.null(inv)){
		message("getting cache...")
		return(inv)
	}
	## Calculate and return cache
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
