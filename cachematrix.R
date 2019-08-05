##these functions work together to speed up the computation of the inverse of a matrix
## by passing "makeCacheMatrix" to "cacheSolve" we can check if an inverse has already been computed



## makeCacheMatrix converts a matrix (which is the argument x) into a list of 
## (set function, the matrix, the inverse of the function if it already exists, and the getInverse function)

makeCacheMatrix <- function(x = matrix(rep(1,4), 2,2)) {
        inverse_x <- NULL
        set <- function(y) {
                x <<- y
                inverse_x <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverse_x <<- inverse
        getInverse <- function() inverse_x
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##cacheSolve checks if there is already an inverse of its argument with an if statement
## if the inverse is empty then it compute the inverse

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
                inverse_x <- x$getInverse()
                if(!is.null(inverse_x)) {
                        message("getting cached data")
                        return(inverse_x)
                }
                data <- x$get()
                inverse_x <- solve(data)
                x$setInverse(inverse_x)
                inverse_x
}
