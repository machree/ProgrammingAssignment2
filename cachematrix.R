## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	invers <- NULL
	set <- function(y){
		x <<-y
		invers<<-NULL
	}
	get <-function() x
	setinvers <- function(solve) invers <<- solve
	getinvers <- function() invers
	list(set=set, get=get, setinvers=setinvers,getinvers=getinvers)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invers <- x$getinvers()
	if(!is. null(invers)){
		message("getting cached data")
		reture(invers)
	}
	data <- x$get()
	invers <- solve(data, ...)
	x$setinvers(invers)

}
