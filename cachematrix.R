## Caching the Inverse of a Matrix
## 
## Rather than repeatedly computing the inverse of a matrix,
## create a special "matrix" object that can cache its inverse

## Example:

# > m <- makeCacheMatrix(matrix(c(4,6,5,9,3,2,1,8,7),3,3))

# > m$get()
#      [,1] [,2] [,3]
# [1,]    4    9    1
# [2,]    6    3    8
# [3,]    5    2    7

# > cacheSolve(m)
#      [,1] [,2] [,3]
# [1,]   -5   61  -69
# [2,]    2  -23   26
# [3,]    3  -37   42

# > cacheSolve(m)
# getting cached data
#      [,1] [,2] [,3]
# [1,]   -5   61  -69
# [2,]    2  -23   26
# [3,]    3  -37   42

# > m$set(matrix(c(2,4,3,1),2,2))

# > m$get()
#      [,1] [,2]
# [1,]    2    3
# [2,]    4    1

# > cacheSolve(m)
#      [,1] [,2]
# [1,] -0.1  0.3
# [2,]  0.4 -0.2

# > cacheSolve(m)
# getting cached data
#      [,1] [,2]
# [1,] -0.1  0.3
# [2,]  0.4 -0.2


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # PRIVATE: cached inverse
        i <- NULL

        # PUBLIC: accessor: set a new matrix
        set <- function(m) {
                x <<- m
                i <<- NULL
        }
        
        # PUBLIC: accessor: retrieve the current matrix
        get <- function() {
                x
        }
        
        # PRIVATE: store the inverse in the cache
        setinverse <- function(inverse) {
                i <<- inverse
        }
        
        # PRIVATE: retrieve the cached inverse
        getinverse <- function() {
                i
        }
        
        # "object" is actually a list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated and the matrix has not changed,
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {

        # retrieve the currently cached inverse if valid
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        # compute the inverse using solve()
        m <- x$get()
        i <- solve(m)
        # cache the value and return the inverse
        x$setinverse(i)
        i
}
