## This code takes any square matrix and finds its inverse, so the final
## output, after calling both functions, is another square matrix. both 
## the matrix and its inverse are stored in cache

## makeCacheMatrix takes a square matrix and stores it in chache. 
## the output is a list of functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv, 
             getinv = getinv)
}


## cacheSolve takes the square matrix stored in cache by makeCacheMatrix 
## and calculates the inverse of that matrix and stores it in cache as well

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                message("valor en caché")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
