## The two functions makeCacheMatrix and cacheSolve can be used in conjuction 
## to cache the output of matrix inverse and save calculation time.
## Input validation is no performed in either of the functions and it is 
## assumed that these functions are called on invertible matrices.


## makeCacheMatrix function accepts an invertible matrix and returns a list of functions 
## which can be used to get, set the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    invMatrix <- NULL
    
    set <- function(y) {                # Set the value of matrix x and reset the inverse
        x <<- y
        invMatrix <<- NULL
    }
    
    get <- function() x                 # return the value of x i.e. the original matrix
    
    setinverse <- function(inverse) invMatrix <<- inverse  # set the value of invMatrix in upper env.
    
    getinverse <- function() invMatrix  # get the value of invMatrix in upper env.
    
    list(set = set, get = get,          # return the list of functions.
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve function takes the output of makeCacheMatrix and uses it 
## get the inverse of the underlying matrix. If already calculated, the
## cached value is returned, else inverse is calculated and cached for future use

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # x is the makeCacheMatrix function output
    
    inverse <- x$getinverse()           # inverse is the matrix inverse of x
    
    if(!is.null(inverse)) {
        message("getting cached data")  # since inverse is already defined return
        return(inverse)
    }
    
    matrix <- x$get()                   # matrix now has the vector from x
    
    inverse <- solve(matrix)            # inverse has been defined with the matrix inverse of x
    
    x$setinverse(inverse)               # cache the value of inverse to be retrieved later
    
    inverse                             # output i.e. inverse
    
}
