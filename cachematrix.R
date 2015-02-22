## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix rather
## than compute it repeatedly.

## The following two functions are used to cache the inverse of a matrix.

## makeVector creates a special "vector",
## which is really a list containing a function to:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
                setinv = setinv, getinv = getinv)
}

## cacheSolve function returns the inverse of the matrix.
## First checks if the inverse has already been computed.
## If so, it gets the result and skips the computation.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data.")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}

## Sample execution:
## > x<- matrix(1:4,2,2)
## > mcm = makeCacheMatrix(x)

## No cache in the first execution
## > cacheSolve(mCM)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## The inverse is already calculated in the second execution
## and is retrieved from the cache
## > cacheSolve(m)
## getting cached data.
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
