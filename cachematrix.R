##Calculate the inverse of inversable matrix and cache the inversion.

## First create a matrix which can be inversed and then calculate the inverse of this matrix, 
##which will be cached in this function.

makeCacheMatrix <- function(x = matrix()) {
	invers <- NULL
	set <- function(y=matrix()){ ##set a matrix to be inversed
		x <<-y
		invers<<-matrix(NULL) ## store the inversed matrix
	}
	get <-function() x
	setinvers <- function(solve) invers <<- solve ## set a inversed matrix
	getinvers <- function() invers ## calculate the inverse of the set matrix
	list(set=set, get=get, setinvers=setinvers,getinvers=getinvers)

}


## Solve the inverse of the matrix created by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invers <- x$getinvers()
	if(!is. null(invers)){ ##if the inversed matrix is already calculated, then directly get the inverse.
		message("getting cached data")
		reture(invers)
	}
	data <- x$get() ##calculated the inverse of the matrix returned from the above function.
	invers <- solve(data, ...)
	x$setinvers(invers)

}
