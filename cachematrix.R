## Genevieve Dupuis - R Programming, Week 3
## Programming Assignment 2

## The two functions below in combination calculate the inverse of a 
## matrix where possible and cache the inverse for quicker retrieval
## of the inverse (as opposed to recalculating it everytime). 

## makeCacheMatrix creates an object that get the value
## of a matix, set it, and get the inverse of a matrix and set it;
## the output of this function is an object that contains a matrix
## and its inverse if it has been calculated already

makeCacheMatrix <- function(x = matrix()) {
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


## cacheSolve either calculates the inverse of the matrix and caches it by 
## assigning the inverse function to the value m OR if the inverse has been
## calculated already (i.e. m in not NULL), it simply pulls the cached inverse 
## out and prints it along with a message indicating as such

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
