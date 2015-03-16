## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## > c = rbind(c(1, -1/4), c(-1/4, 1))
## > class(c)
## [1] "matrix"
## > solve(c)
##          [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
##
## > cc <- makeCacheMatrix(c)
## > cacheSolve(cc)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
##
## > cacheSolve(cc)
## getting cached data
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

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
