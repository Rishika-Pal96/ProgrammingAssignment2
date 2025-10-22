## Creating two functions which will cache the inverse of a matrix and then in the next step calculate the inverse of the matrix and if already present then retrieve the cached inverse 

## Step 1: Function to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        #initialize the object 
        inv <- NULL
        #set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #get the matrix
        get <- function() x
        
        # Set the inverse of the matrix
        setInv <- function(inverse) inv <<- inverse
        
        # Get the cached inverse
        getInv <- function() inv
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Step 2: Retrieve the cached inverse if already present, else solve the inverse of the matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
  
  # If inverse is already cached, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Compute the inverse if not cached
  tab <- x$get()
  inv <- solve(tab, ...)
  x$setInv(inv)
  
  inv
}
