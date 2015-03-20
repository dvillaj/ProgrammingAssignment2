## cachematrix.R
##
## Create a cache matrix object that can be used to
## repeatably solve the inverse of the marix, but only
## calculates the inverse once.
##
## Usage:
##  M <- rbind(c(1, -1/4), c(-1/4, 1))
##
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)
##
##  cacheMatrix$set(M)      # Change the cached matrix.
##  M <- cacheMatrix$get()  # Returns the cached matrix.
##
##  cacheMatrix$setInverse(solve(data, ...)) # Private function containing cached inverse of x
##  cacheMatrix$getInverse()                 # Private function used to get the cached inverse of x

## Create a cacheMatrix object for an invertale matrix.
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setsolve <- function(solve) {
                s <<- solve
        }
        
        getsolve <- function() {
                s
        }
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
