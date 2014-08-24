## Write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( m = matrix() ) {

	## Define initialising condition for inverse
    i <- NULL

    ## This function saves the matrix as x
    ## set: sets matrix and resets cached inverse
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## This way we the get the matrix
    ## get: returns matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## This function sets the inverse of the matrix as i
    ## setinv: saves solve value
    setinv <- function(inverse) {
        i <<- inverse
    }

    ## And now we write a function to get the inverse of the matrix
    ## getinv: returns cached inverse value
    getinv <- function() {
        ## this way we return back the inverse property
        i
    }

    ## Now return back a list of the methods
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## Takes the object of that type as an argument 'x', checks if the inverse value is already
## cached, and if it is returns the cached value; if not, this function calculates the
## inverse for the matrix saved in the 'x', saves it into 'x' cache using method 'setinv'
## and returns the result.
cacheSolve <- function(x, ...) {

    m <- x$getinv()
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    x$setinv(m)
    m
}
