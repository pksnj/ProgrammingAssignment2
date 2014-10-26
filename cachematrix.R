makeCacheMatrix <- function(x = numeric(), nrow = 2, ncol = 2) {
## This function creates a special "matrix" object that can cache its inverse.
## default values for nrow and ncol is set to 2

   m <- NULL
   set <- function(y) {
	x <<- y
	m <<- NULL
   }
   get <- function() { 
	matrix(x, nrow, ncol)
   }

   setinverse <- function(solve) m <<- solve
   getinverse <- function() m
	list(set = set, get = get 
             , setinverse = setinverse,
             getinverse = getinverse )

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

