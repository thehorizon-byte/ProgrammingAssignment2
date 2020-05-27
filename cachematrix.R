## ASSIGNMENT II

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set_x <- function(y) {
        x <<- y
        i <<- NULL
    }
    get_x <- function() x
    set_i <- function(inverse) i <<- inverse
    get_i <- function() i
    list (set_x = set_x, get_x = get_x,
          set_i = set_i, get_i = get_i)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$get_i()
    if (!is.null(i)) {
        message("Congratulations. It was cached. :)")
        return(i)
    }
    i <- solve(x$get_x())
    x$set_i(i)
    i
}

a <- makeCacheMatrix(matrix(rexp(100),10,10))
cacheSolve(a)
cacheSolve(a)
cacheSolve(a)

a$set_x(matrix(rexp(100),10,10))
cacheSolve(a)
cacheSolve(a)
