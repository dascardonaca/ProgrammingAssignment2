## This functions create a "matrix" that contains a matrix and can contain the 
## inverse of the matrix cached, so it is not necesary to compute it every time
## 

## At the end of this file in comments is the code for a matrix to test the functions

## makeCacheMatrix creates the "matrix", wich is a list with the functions get, set, getinverse, and setinverse
## to get the matrix, set the matrix, get the inverse of the matrix and set the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve receive a "matrix" created from the makeCacheMatrix, return the inverse of this matrix
## and in the case of it has not been seted before set it into the "matrix" cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}


'''
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h8 <- hilbert(8); h8
sh8 <- solve(h8)
round(sh8 %*% h8, 3)

A <- hilbert(4)
A[] <- as.complex(A)
solve(A)

aa<-makeCacheMatrix(A)
aa$getinverse()

cacheSolve(aa)
aa$getinverse()
'''
