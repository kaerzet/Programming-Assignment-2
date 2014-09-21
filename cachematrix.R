# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        # m will store the cached inverse matrix
        m <- NULL
        
        # Setter for the matrix
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        # Getter for the matrix
        get<-function() x
        
        # Setter for the inverse
        setmatrix<-function(solve) m<<- solve
        # Getter for the inverse
        getmatrix<-function() m
        # Return the matrix with our newly defined functions
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x=matrix(), ...) {
        m <- x$getmatrix()
        # If the inverse is already calculated, return it
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        # The inverse is not yet calculated, so we calculate it
        matrix <- x$get()
        m <- solve(matrix, ...)
        # Cache the inverse
        x$setmatrix(m)
        # Return it
        m
}
