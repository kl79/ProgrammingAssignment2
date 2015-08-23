## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function builds a list containing four funtions, which can set/get the value of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
      set <- function(y) {   				## set value of matrix
                x <<- y
                m <<- NULL
      }
	get <- function() x				## get value of matrix
	setinv <- function(solve) m <<- solve  	## set value of inverse
	getinv <- function() m				## get value of inverse
	list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix; if the inverse has already been calculated, then cacheSolve returns the value from the cache.

cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if(!is.null(m))					## check whether cached data exists
	{
		message("getting cached data")
		return(m)					## return cached data
	}
	data <- x$get()
	m <- solve(data, ...)				## Calculate inverse
	x$setinv(m)
	m       						## Return a matrix that is the inverse of 'x'
}
