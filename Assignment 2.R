## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function( m = matrix() ) {

    ## Initialize the inverse property
    i <- NULL

    ## Set up matrix
    setmatrix <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Get matrix
    getmatrix <- function() {
    	## Return the matrix
    	m
    }

    ## Set the inverse of matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Get the inverse of matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Return a list of all matrices
    list(setmatrix = setmatrix, 
	   getmatrix = getmatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$getmatrix()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}