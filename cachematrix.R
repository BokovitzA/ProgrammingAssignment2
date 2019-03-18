## These functions will provide the ability to cache a matrix inverse
## if attempting to use the same matrix over again

## The makieCacheMatrix function will create a new object with the ability
## to store a passed matrix, solve for it's inverse, cache the inverse, and 
## return the original and inverted matrix upon request

makeCacheMatrix <- function(x = matrix()) {
    
    invertedMatrix <- NULL
    
    set <- function(y){
            x <<- y
            invertedMatrix <<- NULL
    
        } 
    
    get <- function() x
    
    setInverse <- function(im = NULL) invertedMatrix <<- im
    
    getInverse <- function() invertedMatrix
    
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## This function will invert a matrix or return the cached value
## if it already exists

cacheSolve <- function(x, ...) {
    invertedMatrix <- NULL
        invertedMatrix <- x$getInverse()
        
        if (!is.null(invertedMatrix)) {
            message("Getting cached matrix inverse")
            return(invertedMatrix)
                
        } else {
            data <- x$get()
            invertedMatrix <- solve(data)
            x$setInverse(invertedMatrix)
            
            invertedMatrix
        }
        
        
}
