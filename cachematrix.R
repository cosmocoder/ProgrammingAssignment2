## Functions to calculate and cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL   # this will hold the inverse of the matrix

    # sets the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # this function returns the value of the matrix
    get <- function() x

    # sets the inverse of the matrix
    setInv <- function(i) {
        inv <<- i
    }

    # gets the inverse of the matrix
    getInv <- function() inv

    # return the special "matrix" object
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## Computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated then it returns the inverse from the cache

cacheSolve <- function(x, ...) {

    # check if the inverse has already been cached, and if it is then return it
    inv <- x$getInv()

    if( !is.null(inv) ) {
        message('getting cached inverse')
        return(inv)
    }

    # otherwise proceed with the calculation of the inverse
    data <- x$get()   # get the matrix
    inv <- solve(data, ...)    # calculate the inverse
    x$setInv(inv)    # cache the calculated inverse

    # return the calculated inverse
    inv
}