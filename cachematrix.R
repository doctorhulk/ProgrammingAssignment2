## Below is a pair of functions, which make caching the inverse of 
## a matrix easy. First you need to create a "matrix" object by calling
## the makeCacheMatrix function. Matrix elemens can be passed to the object 
## during creaton (as an argument of the function), or later by calling the set 
## function of the object. To prevent unnecessary computations, the inverse 
## should be always accessed by calling the cacheSolve function.
## 
## Example:
##
## > source("cachematrix.R")
## > mx<-makeCacheMatrix(matrix(c(1,-1,1,2),2,2))
## > cacheSolve(mx)
##           [,1]       [,2]
## [1,] 0.6666667 -0.3333333
## [2,] 0.3333333  0.3333333
## > cacheSolve(mx)
## MSG: Cached data.
##           [,1]       [,2]
## [1,] 0.6666667 -0.3333333
## [2,] 0.3333333  0.3333333
## mx$set(matrix(c(2,2,3,2),2,2))
## > cacheSolve(mx)
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0
## > cacheSolve(mx)
## MSG: Cached data.
##      [,1] [,2]
## [1,]   -1  1.5
## [2,]    1 -1.0

## The function makeCacheMatrix creates an object, which 
## caches a matrix. It's also able to cache the inverse
## of the matrix. Each time the cached matrix changes, 
## its cached inverse is invalidated. The returned object is a
## list of methods. The matrix supplied must always be invertible.

makeCacheMatrix <- function(x = matrix()) {
	s<-NULL
	set <- function(y) {
		x<<-y
		s<<-NULL
	}
	get <- function() x
	setsolve <- function(solve) s<<-solve
	getsolve <- function() s
	list(get=get, set=set, setsolve=setsolve, getsolve=getsolve)
}


## The function cacheSolve computes the inverse of the matrix cached
## in the object returned by the makeCacheMatrix function. The inverse
## is saved inside the object. If the inverse has aready been cached, 
## it's not compted again but retrieved from the cache. 

cacheSolve <- function(x) {
	s<-x$getsolve()
	if(!is.null(s)) {
		message("MSG: Cached data.")
		return(s)
	}
	s<-solve(x$get())
	x$setsolve(s)
	s
}

