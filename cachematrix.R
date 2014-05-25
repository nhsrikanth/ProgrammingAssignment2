## Put comments here that give an overall description of what your
## functions do

# This is an example of Caching Matrix inversion 
#
# Usage:
# > x <- matrix(data, nrow, ncol)          -- Create a matrix x with nrow rows and ncol columns and fill with data
# > x[r,c] <- value                        -- Set any element in matrix to be a specific value
# > x1 <- makeCacheMatrix(x)               -- Instantiate matrix
# > x1$get()                               -- Return the matrix -- THIS IS AN IMPORTANT STEP
# > cacheSolve(x1)                         -- Return the inverse of the matrix
# > cacheSolve(x1)                         -- Subsequent calls return the cached inverse


## Write a short comment describing this function

# Make a Cache of Matrix
# makeCacheMatrix: return a list of functions to:
# 1. Set the matrix
# 2. Get the matrix
# 3. Set the inverse
# 4. Get the inverse

makeCacheMatrix <- function(x = matrix()) {
    # To store cached inverse matrix`
    m <- NULL
    # Function to Set matrix
    set <- function(y) {
              x <<- y
              m <<- NULL
    }
    # Function to Get matrix
    get <- function() x
    # Function to Set Inverse matrix
    setsolve <- function(solve) m <<- solve
    # Function to Get Inverse matrix
    getsolve <- function() m
    # Return Matrix and above defined functions
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)

}


## Write a short comment describing this function
# cacheSolve: Calculate the inverse of the matrix if not already cached. 
# If the inverse is already calculated and cached, return cached inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Inverse matrix
    m <- x$getsolve()
    # If inverse is already cached, print message and return cached inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # If inverse matrix not yet cached then calculate inverse matrix
    data <- x$get()
    m <- solve(data, ...)
    # Cache the inverse matric calculated above
    x$setsolve(m)
    # return the inverse matrix
    m
}
