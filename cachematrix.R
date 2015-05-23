## These functions below are made to create an object that stores the matrix, calculate 
## and cache the inversed matrix for it.

## makeCacheMatrix() - function, that produces a list containing 4 functions: 
## set - to set the matrix
## get - to get the matrix 
## setrev - to store the calculated inversed matrix to cache
## getrev - to get the calculated inversed matrix from cache

makeCacheMatrix <- function(x = matrix()) {
		r <- NULL
		set <- function(y){
			x <<- y
			r <<- NULL		
		}
		get <- function()x
		setrev <- function(solve) 
			r <<- solve
        	getrev <- function() r
        	list(set = set, get = get, setrev = setrev, getrev = getrev) ##return the list of functions
}

## This function produces the inversed matrix for the object that was created with the makeCacheMatrix() function.
## Firstly it checks if there was the cached result. If so, the function stops the computation and returns cached result.
## Otherwise it calculates the inversed matrix for the given data and stores it to the cache.

cacheSolve <- function(x, ...) {
		r <- x$getrev()
		if(!is.null(r)) {	## if the inversed matrix was cached
			message("getting cached data")
			return(r)	## Return the inversed matrix from cache
		}
		data <- x$get()
		r <- solve(data, ...)	## Compute the inverse matrix
		x$setrev(r) 		
		r 	   		## Return a matrix that is the inverse of 'x'
}
