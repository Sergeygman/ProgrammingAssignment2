## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.


## This function create a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse
## 4. get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
        t <- NULL
        set <- function(y) {
                x <<- y
                t <<- NULL
        }
        get <- function() x
        settrans <- function(trans) t <<- trans
        gettrans <- function() t
        list(set = set, get = get,
             settrans = settrans,
             gettrans = gettrans)
}


## This function return inverse matrix. 
## 1. Check that inverse has been calculated. If yes
## 2a. gets the inverse and skip calculation. If not
## 2b. calculate inverse and sets the value in cache set inverse using function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        t <- x$gettrans()
        if(!is.null(t)) {
                message("getting cached data")
                return(t)
        }
        data <- x$get()
        t <- solve(data, ...)
        x$settrans(t)
        t
}