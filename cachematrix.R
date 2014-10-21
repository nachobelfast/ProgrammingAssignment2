## The purpose of this set of functions is to provide 
## a simple implementation of a cached solve() function
## on a matrix.

## This function makes a cache-matrix object.
## It expects a matrix as input and returns a list 
## that represent the operations that can be done over the input matrix.
## Those operations are set(x), get(), setinverse(inverse) and getinverse()

makeCacheMatrix <- function(x = matrix()) {

		xx <- x
		inv <- NULL
		set <- function(y) {
				if (!identical(y, xx))	{
						xx <<- y
						inv <<- NULL
				}
        }
        get <- function() xx
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function applies the solve() function to a cache-matrix object.
## It checks whether exist a computed inverse of the cache-matrix or not,
## and returns a cache copy of the inverse or computes it.
## It expects a cache-matrix as input and further parameters needed for 
## accomplish solve() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
