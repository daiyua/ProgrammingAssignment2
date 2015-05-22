## The function makeCacheMatrix will take a matrix, store it, 
## compute the inverse of the cache, and returen the result.


## This function takes in x, a matrix, store it, and send it
## to the cacheSolve function. After getting the result back 
## from cacheSolve function, and display the result on the
## screen.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setcache <- function(solve) m <<-solve
        getcache <- function() m
        list(set = set, get = get, 
             setcache = setcache,
             getcache = getcache)
}


## This function takes the matrix from the memory. If this matrix
## is exist, a message "getting cached data" will will be printed 
## out. The function then calculates the inverse of the matrix by 
## using function "solve". Set the resulte in cache, reture the 
## cache to makeCacheMatrix.

cacheSolve <- function(x, ...) {
        m <- x$getcache()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setcache(m)
        m
}
