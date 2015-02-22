## Two functions for the Third Week of the R Programming Course.
## This lesson illustrates the use of caching to reduce costly
## calcuations.

## makeCacheMatrix: This function creates a special list object
## that can cache the inverse of a matrix (which is computed 
## seperately by the cacheSolve function).

makeCacheMatrix <- function(x = matrix()) {
    
    ## Defines the four functions stored in the special "vector"
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(x) m <<- x
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## cacheSolve: This function computes the inverse of the special 
## matrix contained in the list returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has 
## not changed), then cacheSolve retrieves the inverse from the cache.
## Note: This function does not check if the matrix is invertible
## and will error out when the solve function is called.

cacheSolve <- function(x, ...) {
    
    ## Returns value stored in getInv for the matrix and if it is
    ## null then it will calculated it.
    m <- x$getInv()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    m <- solve(x$get())
    x$setInv(m)
    m
}