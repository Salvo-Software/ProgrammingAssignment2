## With makeCacheMatrix() I can create a special matrix with getter and setter methods for 
## the matrix values and its inverse, then with cacheSolve() it's possible to calculate (if it's not already done)
##  the inverse of a given matrix created with makeCacheMatrix() and set this value in the cache


## Takes a matrix and creates a "special matrix" (a list) that can store the original matrix value and what will be the cached value (the inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL # The inverse function is reset to NULL every time makeCacheMatrix is called
        
        set <- function(y){     #takes an input matrix
                x <<- y         #saves the input matrix with super assignment
                inv <<- NULL #resets the inverse of the new matrix to NULL
        }
        
        get <- function() {x}  #returns the value of the original matrix
        
        setinverse <- function(solve){ #called by cacheSolve() during his first access 
                inv <<- solve           #store the returned value using superassignment
        }
        
        getinverse <-function() {inv} #getter method for the inverse, will return the cached value
        
        
        #list of the internal methods
        
        list(get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Accesses an object created with makeCacheMatrix(), check if the inverse matrix is already
## calculated for the object. If it's not, calculates it and stores the value in the object.
# Then returns the inverse value either way

cacheSolve <- function(x, ...) {
       
        
        inv <- x$getinverse()
        
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()   #code reached if x$getinverse() returns NULL
        #in this case we have to calculate the inverse matrix
        inv <- solve(data)
        x$setinverse(inv) #store the calculate inverse matrix in x
        inv               ## Return a matrix that is the inverse of 'x' to the code that called this function
}
