## This R file includes functions to calculate and cache the inverse of a matrix
## The file contains two main functions:
## 1) makeCacheMatrix: to create and cache a matrix
## 2) cacheSolve: to calculate the inverse of a matrix using the makeCacheMatrix function
##
## Example code:
##      
##      > f <- matrix( c(1,2,0,0,1,3,4,0,6), 3, 3 )
##      > g <- makeCacheMatrix(f)
##      > cacheSolve(g)


## The function makeCacheMatrix can be used to create a matrix object that can cache its inverse
## The function returns a list containing four functions
## 1) set: to set store a matrix
## 2) get: to retrieve the stored matrix
## 3) setsolve: to store the inverse of a matrix
## 4) getsolve: to retrieve the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        ## x contains the original matrix
        ## m contains the inverse of the matrix
        ## when x changes, m will be set to NULL, to indicate it should be re-calculated
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, 
             setsolve = setsolve, 
             getsolve = getsolve)
}


## The function cacheSolve returns the inverse of a matrix (created via makeCacheMatrix)
## It first checks whether the inverse has already been calculated using the getsolve function.
## 1) If so, the inverse of the matrix is returned directly.
## 2) If not so, the matrix is retrieved, the inverse is calculated, and subsequently stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x is a matrix object created using the function makeCacheMatrix 
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## retrieve the original matrix
        data <- x$get()
        ## calculate the inverse of the matrix
        m <- solve(data, ...)
        ## store the inverse of the matrix
        x$setsolve(m)
        m
}