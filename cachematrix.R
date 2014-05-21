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


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
## This function makes the Invert of a matrix
## if it is cached already then just bring the inverse matrix
## ready calculated and saved  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Return a matrix that is the inverse of 'x'
	  cond<<-0
        m <- x$getinverse ()
	  ## checking if the matrix is not the first time and have being cached
          ##  before
        if(!is.na(m[1,1]))  {
                message("****** Getting Cached Data ******")
                return(m)
        }
        data <- x$get()
	  ## Check that the matrix can be Inverted, if not write a message 
          ## and return
	  tryCatch( {m <- solve(data, ...)},
		error= function(w) 
		{message("The Matrix can not be inverted")
		cond<<-1
		return()})
        x$setinverse (m)
	  if(cond==0){m}
}

## Examples to Run
##b<-matrix(runif(16, 1.0, 7.5),4,4)
##b<-matrix(runif(2500, 1.0, 9.5),50,50)
##b<-matrix(1:16,4,4)  -- This matrix is not Inversavle

## How to Run:
##a<-makeCacheMatrix()
##a<-makeCacheMatrix(b)
##a$get()  
##cacheSolve(a)
