## The Matrix' inverse (aka the Matrix part 4)
    
## This Rscript provides two functions to be used together: makeCacheMatrix and cacheSolve. 
## The goal of these two functions is to take a (base) matrix, 'solve' its inverse, and cache it for later use.
## Additionally, the functions are made so that if a matrix is already solved, 
## the cached inverse will be displayed instead of solving the matrix again.
    
## the makeCacheMatrix function is a function which uses a matrix as input.
## The function then creates four functions necessary to cache the matrix' inverse:
## 1. 'set()' is used to set the base matrix used (the matrix to be inverted).
## 2. 'get()' is used to return the base matrix used.
## 3. 'setinv()' is used to set the inverse matrix (it does not actually invert the matrix!).
## 4. 'getinv()' is used to return the inverse matrix.

makeCacheMatrix <- function(x=matrix()) {
    if (is.matrix(x)==FALSE) {
        stop(
            "the input is not a matrix; this function's execution has stopped"
        )
    }
    ## This line checks and 'stops' the function when x is not a matrix, and provides an explanation for stopping
    ## (note this line is repeated in the set function)
    
    if (ncol(x)!=nrow(x)) {
        stop(
            paste0("This matrix (",
                   nrow(x),
                   ",",
                   ncol(x),
                   ") is not a square matrix; this function's execution has stopped"
            )
        )
    } 
    ## This line checks whether the matrix is ordered as a square
    ## by checking if the amount of columns is equal to the amount of rows
    ## If this is not equal, it stops further execution of the function and provides an explanation for stopping
    ## (note this line is repeated in the set function)
    
    inv <- NULL
    ## when the makeCacheMatrix is used, the inverse matrix is reset to zero.
    ## This practically resets the cache to an empty cache.
    ## If this is not done, a previously created inverted matrix 
    ## would be seen as the solution for the current base matrix.
    ## (note that when the set function is used, this action is also performed)
    
    set <- function(y) {
        if (is.matrix(y)==FALSE) {
            stop(
                "The input is not a matrix"
            )
        }
        if (ncol(y)!=nrow(y)) {
            stop(
                paste0(
                    "This matrix (",
                    nrow(y),
                    ",",
                    ncol(y),
                    ") is not a square matrix; this function's execution has stopped"
                )
            )
        } 
        x <<- y
        inv <<- NULL
    }
    ## The set first checks whether the base matrix is a square matrix.
    ## If not, it provides proper error messages.
    ## If so, it sets the base matrix to 'y' and the inverted matrix to NULL (not solved yet).
    
    get <- function() {x}
    ## the get function returns the base matrix.
    
    setinv <- function(solve) {inv <<- solve}
    ## the setinv function sets a solved matrix in the cache as the inverted matrix.
    
    getinv <- function() {inv}
    ## the getinv function returns the inverted matrix from the cache.
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    ## This line lists the function names,
    ## which is necessary in order to call the functions outside the makeCachematrix function environment.
}

## The cacheSolve function requires the list returned by the makeCachematrix function as input x
## Further it accepts arguments for the solve function.
## The cacheSolve function is a two step process.
## First it recovers the getinv function from the makeCachematrix function and uses it to return the inverse matrix.
## Then, if the matrix has already been solved and cached previously, 
## the cachesolve function provides a message it is obtaining cached data
## and returns the cached inverse matrix.
## Else (if matrix' inverse has not been solved yet) the matrix is solved and the inverse matrix is returned.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    ## this retrieves the getinv function from the makeCacheMatrix function, which needs to be stored in object x.
    ## and uses it to read the cache for a previously inverted matrix
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## this checks whether the object inv (in this functions environment) is NOT empty.
    ## If so, it returns the inverted matrix
    ## if not, the following code is used to return an inverted matrix.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
## Finally, a matrix that is the inverse of 'x' is returned.
## 
## Example use:
## c<-matrix(1:4,nrow=2,ncol=2)
## c
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## x<-makeCacheMatrix(c)
## cacheSolve(x)
## will return:
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
