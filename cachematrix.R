## These functions are designed to cache the inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse
}

## This function computes the inverse of the special "matrix" returned by the makeCacheMatrix function above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve function below should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
        	message("getting inversed cached data")
        	return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
}
