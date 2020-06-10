## Creates a special "matrix" object to be cached with 
## environment info for later use.

makeCacheMatrix <- function(x = matrix()) {
        
        ## 'x' is of class matrix() that will be converted into a 'special' matrix,
        ## that will be passed to the cacheSolve() function to be inverted
        
        ## Returns an object that contains the special matrix with env. details
        
        s <- NULL ## initialize object for solved matrix
        
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y ## Assign the input argument to x in the parent environment
                s <<- NULL
        } ## end of set function
   
        ## Get the value of the matrix 
        get <- function() x
        
        ## Create solve function for environment to be called if cached
        setSolve <- function(solve) s <<- solve
        
        ## Function to be called to get solved matrix
        getSolve <- function() s
        
        ## Return special matrix with environment functions
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
        
} ## end of function

## Function returns a solved matrix from either a previously cached
## object or solves the passed matrix

cacheSolve <- function(x) {
        
        ## 'x' is an object of type makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'

        data <- x$getSolve()
        ## if matrix is cached, then retrieve it
        if(!is.null(data)) {
                 message("Getting cached data")
                 return(data) ## Return inverted matrix
        } else {
                data <- solve(x$get())
                ## set the cached solve matrix
                x$setSolve(data)
                data ## Return inverted matrix
                
        } ## end of if

} ## end of cacheSolve function

