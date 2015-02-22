## Finding the inverse of a matrix is a computationally costly operation.
## This function saves on computation time by caching the value when it's first required. 
## When the inverse is required again the function will return the previously computed value if the value of the matrix is unchanged.

## This function creates a special matrix object that caches it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        m_inverse <- NULL
        set <- function(y) {
                x <<- y
                m_inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m_inverse <<- solve
        getinverse <- function() m_inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function gives the inverse of a matrix. If the inverse of the matrix has not been previously computed it will call the makeCacheMatrix
## function to make a special matrix object and cache it. If the inverse has been computed it retrieves the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m_inverse <- x$getinverse()
	if (!is.null(m_inverse)){
		message("Retrieving cached value")
		return(m_inverse)
	}
	data <- x$get()
	m_inverse <- solve(data, ...)
	x$setinverse(m_inverse)
	m_inverse
}
