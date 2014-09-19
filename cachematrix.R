### Thanks in advance for peer assessing!


## "makeCacheMatrix" (1) sets & gets the values of the matrix
## and (2) sets and gets the values of the matrix's inverse.


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function() m
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## "cacheSolve" calculates the inverse of the matrix made in
## "makeCacheMatrix" only if it has not yet been calculated.
## It first checks the cache to see is the matrix has been
## solved, and, if so, gets the inverse matrix from the cache
## instead of recalculating.


cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}


## The below was for reference.
#
#
#
# makeVector <- function(x = numeric()) {
#    m <- NULL
#    set <- function(y) {
#       x <<- y
#       m <<- NULL
#    }
#    get <- function() x
#    setmean <- function(mean) m <<- mean
#    getmean <- function() m
#    list(set = set, get = get,
#         setmean = setmean,
#         getmean = getmean)
# }
# 
# cachemean <- function(x, ...) {
#    m <- x$getmean()
#    if(!is.null(m)) {
#       message("getting cached data")
#       return(m)
#    }
#    data <- x$get()
#    m <- mean(data, ...)
#    x$setmean(m)
#    m
# }