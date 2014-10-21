## These functions return the inversion of the original matrix. The first time 
## it has to be calculated, any other time it will return the cached inversion.
## Precondition: The matrix has to be invertible.

## The makeCacheMatrix function creates a list of functions which allows 
## to set and get the matrix and its inverted matrix and caches them.

makeCacheMatrix <- function(x = matrix()) {
        
        ### initialisation of the inversed matrix
        inv_x <- NULL 
        
        ### sets the original matrix and deletes (initializes) the inversed matrix
        set <- function(y) {    
                x <<- y         
                inv_x <<- NULL
        }
        
        ### gets the original matrix
        get <- function() x     
        
        ### sets the inverse matrix
        setinv_x <- function(inversematrix) inv_x <<- inversematrix
        
        ### gets the inverse matrix
        getinv_x <- function() inv_x
        
        ### returns list of functions
        list(set = set, get = get, setinv_x = setinv_x, getinv_x = getinv_x)
        
}


## The cacheSolve function returns the inverse of the matrix, either by 
## calculation (solve) or by returning the cached inversion. It is
## using a makeCacheMatrix object which holds the matrix and, 
## if once calculated, the inverse matrix.

cacheSolve <- function(x, ...) {
        
        ### gets the inverse of the matrix
        inv_x <- x$getinv_x()
        
        ### if the inverse of the matrix is found, return inverse
        if(!is.null(inv_x)) {
                message("getting cached data") ### only for test purpose
                return(inv_x)
        }
        
        ### in the other case (inverse not found), get the matrix,
        ### calculate the inverse, save it into the cache and return
        ### the value
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinv_x(inv_x)
        inv_x ### return
        
}
