## The function makeCacheMatrix takes as argument 
## an invertible square matrix.  

## The output of makeCacheMatrix is 
## a list of length 4 that is 
## something like a helpdesk at a library 
## that can performs four functions for you 
## when you request the inverse of a matrix.

## The function cacheSolve takes as argument an 
## output of the function makeCacheMatrix.

## The output of cacheSolve is
## the inverse of the matrix that was the input for makeCacheMatrix.

## Rationale for these two functions is this:
## Calculating the inverse of a square matrix is time consuming
## so you do not want to compute the same inverse many times.

## If you are going to need the inverse of a matrix many times,
## it is preferable to do the following:

## (1) The first time you need the inverse 
##  -calculate the inverse
##  -store (cache) the inverse 
##  -create a record that tells where the inverse is cached
##  -return the matrix inverse

## (2) Subsequent times you need the inverse 
##  -check for a record of the inverse
##  -if the record shows this is the first time you requested it
##      --go to (1) above. 
##  -if the record shows that the inverse has been requested before
##      --retrieve the matrix inverse from the cache location
##      --return the inverse

## The function makeCacheMatrix records the info that enables 
## a call from cacheSolve to initiate the sequence of steps above. 

makeCacheMatrix <- function(x = matrix()) {
        ## Argument: x is an invertible square matrix
        
       
        
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




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ... )
        x$setsolve(m)
        m
        
        
        
}
