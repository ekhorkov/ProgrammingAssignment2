## Setting and getting matrix and its solve in memmory

makeCacheMatrix <- function(x = matrix()) {
   
    set <- function(inMatrix) {
        x <<- inMatrix
        msolved <<- NULL
    }
    get <- function() x
    
    setsolve <- function(s) {
        msolved <<- s
    }
    
    getsolve <- function() msolved
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


cacheSolve <- function(m, ...) {
    matrixsolve <- m$getsolve()
    
    #check if solve exists
    if(!is.null(matrixsolve)) {
            return(matrixsolve)   
    }
    browser()
    data <- m$get()
    newsolve <- solve(data, ...)
    m$setsolve(newsolve)
    newsolve
}
