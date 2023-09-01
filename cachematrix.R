makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv_matrix <<- inverse
    getinverse <- function() inv_matrix
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    inv_matrix <- x$getinv()
    if (!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    data <- x$get()
    inv_matrix <- solve(data, ...)
    x$setinverse(inv_matrix)
    inv_matrix
}

# Create a matrix using makeMatrix
mat <- makeCacheMatrix()

# Set values of the matrix
mat$set(matrix(c(2, 3, 5, 9, 10, 4, 8, 5, 4), 3, 3))


# Calculate and cache the invere
cacheSolve(mat) 

# Access the cached inverse without recalculating
cacheSolve(mat)