## Create a matrix with methods to get and set from cache the matrix itself and its inverse.
## Return the matrix object created
makeCacheMatrix <- function(matrix = matrix()) {
    inversedMatrix <- NULL
    
    set <- function(y) {
        matrix <<- y 
        inversedMatrix <<- NULL 
    }
    
    get <- function() {
        matrix
    }
    
    setInversedMatrix <- function(newMatrix) {
        inversedMatrix <<- newMatrix
    }

    getInversedMatrix <- function() {
        inversedMatrix
    }
    
    list(
        set = set, 
        get = get, 
        setInversedMatrix = setInversedMatrix, 
        getInversedMatrix = getInversedMatrix
    )
}


## Return the inverse of a matrix. 
## The value is returned from cache if it exists.
## Otherwise it will be calculated
cacheSolve <- function(x, ...) {
    inversedMatrix = x$getInversedMatrix()
    
    if(!is.null(inversedMatrix)) {
        message("Returning the cached inversed matrix")
        return(inversedMatrix)
    }
    
    inversedMatrix = solve(x$get())
    
    x$setInversedMatrix(inversedMatrix)
    
    inversedMatrix
}