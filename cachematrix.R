


makeCacheMatrix <- function(x = matrix()) {

# Matrix invese does not exist at the beggining, so a NULL value is assigned
inverse <- NULL

# Set method
set <- function(y){
  x <<- y
  inverse <<- NULL
}

# Get method
get <- function(x){x}

# Set Inverse method
setInverse <- function(inv){inverse<<-inv}

# Get inverse method
getInverse <- function(){inverse}

list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



cacheSolve <- function(x, ...) {
    # Get inverse attribute from object x
     inverse <- x$getInverse()
     
     # If the attribute exists (value not equal to NULL) return the value obtained
     if(!is.null(inverse)) {
       message('getting cached data')
       return(inverse)
     }
     
     # If the value doesn't exist, calculate the inverse, set it and return it
     mat <- x$get()
     inverse <- solve(mat, ...)
     x$setInverse(inverse)
     
     return(inv)
}
