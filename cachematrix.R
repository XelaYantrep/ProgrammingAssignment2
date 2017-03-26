## cacheSolve is a variation on solve(M) for a matrix M where the inverse is
## stored once calculated.  makeCacheMatrix is a variation on the matrix object
## which is required to use the cacheSolve function.

## makeCacheMatrix takes any matrix x as a parameter and returns a list with
## all the functions that cacheSolve requires.  The purpose of storing the
## data as a makeCacheMatrix is to associate the inverse with the data.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(z) inv <<- z
    getinv <- function() inv
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve requires a makeCacheMatrix object to be passed in.  It then 
## checks if the matrix inverse has been previously calculated, calculates it
## if not, and returns the matrix inverse regardless.  Additional parameters 
## may be passed in, which will be relayed to the solve function for a matrix.
## Example: you may pass in b = MyMatrix if you wish to solve a %*% x = b for 
## a nonidentiy matrix.
##
## Note: it is assumed that the matrix is invertible, you will get the standard
## "system is computationally singular..." message if not.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("retrieving cached data")
        return(inv)
    }
    message("no cached value found, calculating...")
    mat <- x$get()
    NewInv <- solve(mat, ...) #identity matrix is default second param.
    x$setinv(NewInv)
    NewInv
}

## Some example code follows to demonstrate the proper use of these functions:
#
# B = matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE)
# spB <- makeCacheMatrix(B)
# cacheSolve(spB) #This will calculate the inverse
# cacheSolve(spB) #This will recall the inverse from memory, saving computation.

# Code skeleton attributed to Roger Peng, details Xela Yantrep.