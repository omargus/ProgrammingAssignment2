## Put comments here that give an overall description of what your
## functions do

##This function takes an invertible matrix x as an argument.
##The function outputs a list of four items, all functions.
## set sets the value of the matrix to x.
## get retrieves the matrix.
## setinv sets the value of the inverse matrix.
## getinv gets the value of the inverse matrix.
## These functions are then called from the CacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


##This function first checks if there is already an inverses
##matrix available by calling x$getinv()
##If the data is available the cached inverse matrix is returned.
##Otherwise the matrix is retrieved using x$get
##then solved using solve()
##and teh cached inverse matrix is set using x$setinv.
##finally the  inverse matrix (i) is returned

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
