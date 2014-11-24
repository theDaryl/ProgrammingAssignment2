## Put comments here that give an overall description of what your
## functions do

## 
## This function creates a special matrix object that is then cached.
## When called it creates inver
##

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m<<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## 
## This function checks for a cached version of the matrix
## If it is not cached it computes the inverse of the matrix using the solve() function
## This function assumes that the matrix is invertible (square dimensions)
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        print(m)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        message("are we here")
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
