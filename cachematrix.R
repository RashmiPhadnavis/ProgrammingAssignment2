# The following pair of functions cache the inverse of a matrix.

# makeCacheMatrix creates a special "matrix" object,which is really a list
# containing functions to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the given matrix
# 4. get the value of the inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){
                x
        }
        setinverse <- function(inverse){
                inv <<- inverse
        }
        getinverse <- function(){
                inv
        }
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)

}


# cacheSolve calculates the inverse of the special "matrix" created with 
# makeCacheMatrix function.
# It first checks to see if the inverse has already been calculated.
# If yes, skips the computation and gets the value from cache.
# The input matrix should be a square invertible matrix.


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
