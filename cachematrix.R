## In order to speed up the computation of the inverse of an invertible matrix, we will create a new matrix which caches it's own inverse
## As long as the original matrix does not change rather than computing the inverse of the original matrix all the time, we rely on the cached value
## so that the inverse matrix is calculated only once 


## Defines setters and getters of the matrix and its cached inverse

makeCacheMatrix <- function(x = matrix()) {

   ## Initializes the property that stores the inverse matrix
   invMat <- NULL
   
   ## Stores the matrix and resets the invMat property
   setMatrix <- function( matrix )
   {
     mat <<- matrix
     invMat <<- NULL
   }
   
   ## Retrieves the matrix
   getMatrix <- function()
   {
     mat
   }
   
   ## Stores (caches) the inverse of the matrix
   setInverseMatrix <- function(inverseMatrix)
   {
     invMat <<- inverseMatrix
   }
   
   ## Retrieves the inverse of the matrix
   getInverseMatrix <- function()
   {
     invMat
   }
   
   ## Returns all methods as a list
   list(setMatrix = setMatrix, getMatrix = getMatrix,
		setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}



## Returns the inverse of a matrix 'x'. 
## If it has been computed already, it returns the cached value

cacheSolve <- function(x, ...) {
    ## Retrieves the inverse of the matrix, which defaults to NULL
   invMatrix <- x$getInverseMatrix()
   
    ## if it's not null it returns it 'cause it's the cached value
   if (!is.null(invMatrix))
   {
	 message("Retrieving a cached value")
     return(invMatrix)
   }
   
    ## Retrieves the matrix provided to the makeCacheMatrix 
   mat <- x$getMatrix()
   
    ## computes the inverse matrix
   invMatrix <- solve(mat, ...)
   
    ## caches it
   x$setInverseMatrix(invMatrix)

   ## and returns it
   message("New value computed and cached")
   invMatrix
}
