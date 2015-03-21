## Together, these functions allow the caching of a matrix inverse. The function
## 'makeCacheMatrix' takes as an input a matrix, and caches the inverse of
## the matrix. This inverse of the matrix can then be called at a later time
## when needed, using the function 'cacheSolve'.


## This function 'makeCacheMatrix' creates a matrix object. It 
## works in conjunction with function 'cacheSolve' to cache the inverse value 
## of the matrix, to be called at a later time.

makeCacheMatrix <- function(matrix1 = matrix()) {
##initialize the cached matrix object to NULL
        stored_matrix <- NULL
        
        ##Used to set the value of the matrix to y, and the cached matrix value
        ## to NULL in the 'makeCacheMatrix' function. 
        SetMatrixValue <- function(y) {
                matrix1 <<- y
                stored_matrix <<- NULL
        }
        ##used to call the stored value of the matrix
        
        GetMatrixValue <- function() {
                return(matrix1)
        }
        ## Used in conjunction with function 'cacheSolve' to set the value of
        ## the cached inverse of the matrix, object 'stored_matrix'.
        
        setinverse <- function(sent_matrix) stored_matrix <<- sent_matrix
        
        ## Used in conjunction with the function 'cacheSolve' to retrieve the
        ## cached matrix inverse 'stored_matrix'.
        
        getinverse <- function() stored_matrix
        
        ##Print a list of objects in the environment that can be called
       
        list(SetMatrixValue = SetMatrixValue, GetMatrixValue = GetMatrixValue, 
             setinverse = setinverse, getinverse = getinverse)
}




## This function returns a matrix that is the inverse of 'x'. The function first
## checks to see whether the inverse has already been calculated, by pulling
## the cached matrix from the function 'makeCacheMatrix' into local object
## 'matrix 2'. If this local value 'matrix2' is not NULL (and therefore an 
## inverse value has already been cached), then the inverse matrix 'matrix2' is 
## returned. Otherwise, the matrix value is called into a local object 'data', 
## and the inverse value is then calculated and assigned to the local 'matrix2' 
## object. This value is both sent back to the cache and printed.

cacheSolve <- function(x, ...) {
        matrix2 <- x$getinverse()
        
        ## Check to see if there is a value already stored
        
        if(!is.null(matrix2)) {
                message("getting cached data")
                return(matrix2)
        }

        ## If no cached matrix inverse is available, calculates one locally.
        
        else {
                data <- x$GetMatrixValue()
                matrix2 <- solve(data, ...)
        

        ## Set the cached value (from function 'MakeCacheMatrix' to be the 
        ## matrix inverse 'matrix2' calculated locally

                x$setinverse(matrix2)
        
        ## Print the the locally calculated matrix inverse
        
                return(matrix2)
        }
}
