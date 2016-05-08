## create a list (special type of matrix object) containing 
## set/get/setinv/getinv functions associated with 
## computing inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invx) inv <<- invx
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## calculate the inverse of the special type of matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## Compute the matrix that is the inverse of 'x', and save it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}