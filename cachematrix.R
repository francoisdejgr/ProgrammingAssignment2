## This function contains getter and setter functions.  It hands over the ablity to caluclate
# to the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # clear m
    set <- function(y) 
        {
            #setter function
            x <<- y #push to parent - so that we can grab it
            m <<- NULL # clear parent m
        }
    #getter function : get values from parent
    get <- function() x 
    
    setMatix <- function(solve) m <<- solve
    getMatix <- function() m # retieve from parent
    
    #These names are used in the Solve function to refer to the functions by name
    list(set = set,          # gives the name 'set' to the set() function defined above
         get = get,          # gives the name 'get' to the get() function defined above
         setMatix = setMatix,  # gives the name 'solveMatix' to the solveMatix() function defined above
         getMatix = getMatix  # gives the name 'getMatix' to the getMatix() function defined above
        )
}

## This function either solve the matix or if it was already solved return the value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getMatix() #in x there is now a function 
    if(!is.null(m)) 
        {
            message("getting cached data")
            return(m)
        }
    data <- x$get() #trigger the getter function 
    m <- solve(data, ...)
    x$setMatix(m)
    print(m)
}

