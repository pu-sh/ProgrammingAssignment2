## The makeCacheMatrix function returns a vector 
## containing a special list of functions that can
## 1. cache the value of a matrix
## 2. retrieve the cached value of the matrix
## 3. cache the inverse  of the matrix
## 4. retrieve the cached inverse of the matrix 
## The makeCacheMatrix function takes a single
## argument var_cachedMatrix of class "matrix"
## and returns a list of above functions that become
## associated with the var_cachedMatrix variable

makeCacheMatrix <- function(var_cachedMatrix = matrix()) 
{
    ## Declare a variable to cache the inverse of the matrix 
    ## and set the variable to NULL
    var_cachedInverse <- matrix()
    var_cachedInverse <- NULL
 
    #############################################################
    ## Cache the matrix that passed via the "set" function below
    #############################################################
    
    set <- function(var_matrix) 
    {
        var_cachedMatrix <<- var_matrix
        
        ## Since the matrix that was cached in this function has changed,
        ## we need to set the inverse of the earlier matrix to NULL
        var_cachedInverse <<- NULL
    }
    
    #############################################################
    ## Get the cached matrix that is stored in this function
    #############################################################
    
    get <- function()
    {
        ## Simply return the var_cachedMatrix variable that has
        ## been set via the "set" function. If the variable has
        ## not been set at this point, it will return the default 
        ## value, i.e., NULL
        var_cachedMatrix    
    }

    #############################################################
    ## Set the inverse of the matrix through the function below
    #############################################################
    
    setInverse <- function(var_inverse) 
    {
        ## Note that before this function is called, it is assumed
        ## that the matrix whose inverse is being calculated was
        ## extracted from makeCacheMatrix via the "get" function,
        ## otherwise the matrix will not match the inverse being
        ## assigned below
        var_cachedInverse <<- var_inverse
    }
    
    #############################################################
    ## Get the inverse of the matrix through the function below
    #############################################################
    
    getInverse <- function()
    {
        ## Simply return the var_cachedInverse variable that has
        ## been set via the "setInverse" function. If the variable has
        ## not been set at this point, it will return the default 
        ## value, i.e., NULL
        var_cachedInverse
    }
    
    ## Create a vector list of functions based on the definitions above
    ## and return the list using the below code. The functions can be 
    ## accessed by typing the following code:
    ## > testMatrix <- makeCachedVector(var_matrixtoInverse)
    ## > testMatrix$set(var_matrixtoInverse) - this sets the matrix
    ## > testMatrix$get() - this gets the matrix
    ## > testMatrix$setInverse(var_InversedMatrix) - this sets the inverse
    ## > testMatrix$getInverse() - this gets the inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
} 


## The cacheSolve function takes an intvertible matrix  
## as input and returns its inverse. Internally,
## the function first checks if the inverse of the
## matrix has already been calculated by using the
## getInverse method of the makeCacheMatrix object. If the
## inverse exists, a cached copy of the inverse is returned
## without recalculating the inverse, otherwise the inverse
## is calculated, cached in memory using the setInverse
## method of the makeCacheMatrix object, and then returned to
## the calling function

cacheSolve <- function(var_MakeCacheMatrixObj, ...)
{ 
    ## Declare a matrix variable and assign the inverse of the 
    ## original matrix retrieving the inverse using the getInverse
    ## function of the var_MakeCacheMatrixObj object
    var_inverse <- var_MakeCacheMatrixObj$getInverse()
    
    #############################################################
    ## Check if var_inverse is NULL, i.e., it is being calculated
    ## for the first time
    #############################################################
    
    if(!is.null(var_inverse))
    {
        ## The inverse is not NULL, i.e., the inverse exists
        ## In that case, there is no need to calculate the inverse
        ## again. The function can return the inverse and exit
        message("Getting a cached copy of the inverse")
        
        ## Return the inverse and exit the function
        return(var_inverse)
    }
    
    #############################################################
    ## var_inverse is NULL. We need to calculate the inverse and
    ## cache it in memory using the setInverse method of the
    ## var_MakeCacheMatrixObj object
    #############################################################
    
    # Get the matrix from the var_MakeCacheMatrixObj object and
    # cache it in the var_retrievedMatrix variable
    var_retrievedMatrix <- var_MakeCacheMatrixObj$get()
    
    # Calculate the inverse using the solve() function in R
    var_inverse <- solve(var_retrievedMatrix, ...)
    
    # Cache the inverse using the setInverse method
    var_MakeCacheMatrixObj$setInverse(var_inverse)
    
    # Finally, return the inverse to the calling function
    var_inverse
}