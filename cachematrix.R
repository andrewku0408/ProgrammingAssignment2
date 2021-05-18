## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ##set the value of the matrix
        set <- function (y){
                x <<- y
                inv <<- NULL
        }
        ##get the value of the matrix
        get <- function() {x}
        ##set the value of the inverse matrix
        setInv <- function(inverse) {inv <<- inverse} 
        ##get the value of the inverse matrix 
        getInv <- function() {inv}
        list(set = set, get = get,
             setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
                message("getting cashed data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}

