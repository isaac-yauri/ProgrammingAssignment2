## Sometimes we need to repeat calculations that take a long time. 
## The idea is to demonstrate the use of a new function to store 
## results required several times and prevent calculate again.
##
## This time , the solve() function will be used to calculate the inverse of a matrix.
## And cacheSolve() will automatically detect if the inverse of the matrix was alreadu
## calculated and stored.
##
## If you have a new matrix , then it will calculate and store the result for future reference.
##
## Function makeCacheMatrix create a list to store the original matrix and the inverse.
## Example:
##
##  Creating a Matrix
##     c <- matrix(sample.int(100,size=100,replace=TRUE), nrow=10)
##
##  Using this function to save this matrix and reserve space for its inverse matrix.
##
##     m <- makeCacheMatrix(c)
##
##  To get the original matrix:
##
##     m$get()
##

## This is the function makeCacheMatrix to store the original matrix.
## For now, we store NULL as the inverse of the matrix, because we don´t know if we really need to calculate 
## its inverse.
## On this function, we create 2 methods.
##
## setsolve: to store a value
## getsolve: to get value that was already stored.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    get <- function() { x }
    setsolve <- function(solve) { s <<- solve }
    getsolve <- function() { s }
    list(get = get, setsolve = setsolve, getsolve = getsolve)
}

##  After save our Matrix, we need to calculate its inverse
##  
##  To retrieve the current value of the inverse stored, we use:
##
##       m$getsolve()
##
##  We receive a NULL value, because didn't calculate the inverse yet.
##
##       CacheSolve(m)
##
##  When you use this function for the first time (in this case for the variable "m"), 
##  function calculates inverse of a matrix and store.
##
##  Whe you use this function for the second time (and so on), this function
##  detects that was already calculated and stored, so just shows this stored value.
##
##       CacheSolve(m)
##       getting cache data
##       ... inverse of the matrix ...
##


cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
