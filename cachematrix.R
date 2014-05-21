## By Abraham Aizenman
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	## Define an empty Matrix
	  m <- matrix(,nrow(x),ncol(x))
	  t <- matrix(,nrow(x),ncol(x))
        set <- function(y) {
                x <<- y
                m <<- t
        }
        get <- function() x
	#Call the set of the Inverse
          setinverse <- function(solve) m <<- solve
	#Call to get of the Inverse
          getinverse <- function() m
	# Lists of functions for grouping functions in 
	# Matrix
          list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
