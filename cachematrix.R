#function that creates a special "matrix" object to be used to cache its inverse
makeCacheMatrix <- function (x = matrix()){
    
    inv  <- NULL
    set <- function(y) {
        
        x <<- y
        inv <<- NULL
    }
    
    get <- function () x
    setinv <- function (inverse) inv <<- inverse
    getinv <- function() inv 
    
    list (set = set, get = get, setinv = setinv,getinv = getinv)
}

# calculates inverse matrix
cacheSolve <- function (x, ...) {
    
    inv <- x$getinv()    
    matx<- x$get()                 # will be used to check if matrix has changes
    
    
    if(!is.null(inv)) {
        
        p <- sum(solve(matx)== inv)  # if inv is not null checks whether matrix has changed 
        cnt <- sum(dim(inv))
        
        if(p==cnt)  {              # if matrix hasn't changed returns stored inverse
            
            message("getting cached inverse matrix")
            return(inv)
        }
        
    }
    
    data <- x$get()                # gets the matrix for which the inverse will be calculated 
    inv <- solve(data, ...)          # calculates inverse of matrix
    x$setinv (inv)                   # sets inverse
    inv
}