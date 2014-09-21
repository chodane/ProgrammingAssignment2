##Since this assignment is largely cut-and-paste from the
##sample code with only minor revisions, I have added
##extensive comments in order to demonstrate a basic
##understanding of the assignment

## These functions allow cached solving of matrix inverses

## The makeCacheMatrix function creates a special list object
## for each square matrix that it is fed. The resulting list
## contains functions for either setting or getting the matrix
## itself and its inverse. Because these functions are created
## within the makeCacheMatrix function, any variables defined
## by the functions in this list are effectively cached in the 
## makeCacheMatrix environment.

makeCacheMatrix <- function(x = matrix()) {
        ##The single argument is the matrix you wish to cache-solve
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }       ##the set function when called will reset the value of x
                ##for the parent (makeCacheMatrix) environment. When it is 
                ##changed, the inverse will be cleared out to prevent 
                ##existing data from being mistaken as the inverse
        
        get <- function() x
                ##merely returns the value of the matrix stored
        
        setinverse <- function(inverse) i <<- inverse
                ##stores the value of the inverse in the makeCacheMatrix environment.
                ##Hopefully it is only ever called by the cacheSolve function!
        
        getinverse <- function() i
                ##merely returns the value of the inverse stored
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
                ##the object created is a list of the four functions
}


## This function will call on the functions in the list created by the function
## above in order to call up a cached inverse for a matrix or calculate and cache 
## one if it does not already exist.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
                ##calls the function from the list created above that returns
                ##the inverse matrix
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }       ##if the inverse has already been calculated, then a message
                ##is returned informing the user that it is using cached data
                ##returns the inverse, and exits the function
        
        data <- x$get()
        i <- solve(data,diag(dim(data)[1]), ...)
        x$setinverse(i)
                ##otherwise, the matrix is called, the inverse is calculated,
                ##and then cached in the makeCacheMatrix environment via setinverse
        
        i
        ## Returns a matrix that is the inverse of 'x'
}
