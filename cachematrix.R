## matrix inversion is costly in term of computation, 
## that is why we want to cache the inverse of the matrix 
## than just compute it everytime.
## below functions used to store the matrix and to cache the inverse
## of it.

## caching inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inver <-  NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    
    list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This cachesolve function must return the inverse of a matrix 
## which is created in above function.
## if it finds that matrix given is already inverted then 
## it should return a cahched inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inver <- x$getinverse()
    if(!is.null(inver)) {
        message("aquiring cached data ")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data)
    x$setinverse(inver)
    
    inver
}
