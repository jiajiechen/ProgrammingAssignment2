## This makeCacheMatrix function aims to cache solved inverse matrix

## This makeCacheMatrix function has two variables
# The first one is x, the target matrix
# The second is cache m.

#define the variables in the function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
#This function searches y in the global environment and apply it to x
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



# In this function, x is the result returned by makeCacheMatrix.
# Hence, if the matrix has been solved the cacheSolve function will
# return the solved result; however, if the result has not been solved
# the cacheSolve function will call solve() and at the same time save
# the solved result into makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
