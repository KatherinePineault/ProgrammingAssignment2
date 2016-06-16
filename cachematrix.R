## When used together, makeCacheMatrix and cacheSolve will be used to find the inverse of an inversible
## matrix. It will reduce the computation time by only computing the inverse of such matrix if it hasn't 
## been computed previously.

############EXAMPLE ######################
# Ff we set the inversible matrix "Mat" 
                #Mat<- matrix(c(1,2,3,4,5,-6,7,8,9),3,3)
## and then run the functions together
                #cacheSolve(makeCacheMatrix(Mat))
## it will return the inverse of the matrix Mat: 
                #            [,1]       [,2]        [,3]
                #       [1,] -1.29166667  1.0833333  0.04166667
                #       [2,] -0.08333333  0.1666667 -0.08333333
                #       [3,]  0.37500000 -0.2500000  0.04166667

##########################################


## The makeCacheMatrix function creates a "vector" of functions that can be used to set and get a matrix
## and set and get the Inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        
        set <- function(y)
        {
                x <<- y
                Inv <<- NULL
        }
        
        get <- function()
        {
                return(x)        
        } 
        
        setInverse <- function(Inverse)
        {
                Inv <<- Inverse
        }
        
        getInverse <- function()
        {
                return(Inv)        
        } 
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}





## The cacheSolve function returns the matrix that is the inverse of "x". Before computing the inverse of
## the given matrix, it searches to see if the inverse of such matrix has already been computed. If it was
## already computed, it just returns the inverted matrix. If not, it uses the function solve to compute the
## inverse of the matrix, and returns it. 

cacheSolve <- function(x, ...) {
        Inv <- x$getInverse()
        if(!is.null(Inv)) {
                message("getting cached matrix inverse")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInverse(Inv)
        Inv
}
