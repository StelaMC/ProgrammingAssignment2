## makeCacheMatrix creates a list containing functions to set and get the value of the given matrix, and set and get the value of its inverse. cacheSolve checks it the inverse of the given matrix has been stored, and, if yes, returns the inverse. If not, it calculates it from the scratch and stores it for future use.

## makeCacheMatrix creates functions which are called by the cacheSolve function. These functions are: set (rewrites the function with new values), get (which gets the values of a given matrix), setinverse (calculates the inverse of a given matrix) and getinverse (caches the inverse of a given matrix). It also creates an empty vector in which the result of the calculation (i.e. inverse of the matrix) is stored.


makeCacheMatrix <- function(x = matrix()){
        invr <- NULL

        set <- function(y) {
                x <<- y
                invr <- NULL
        }
        get <- function() { x }
        setinverse <- function(solve) { invr <<- solve }
        getinverse <- function() { invr }
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calls the makeCacheMatrix functions to see if there inverse for the given matrix has been stored. If yes, it returns it with the "getting cached data" message. If not, the inverse is calculated anew and returned, with it's value cached for the future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        invr <- x$getinverse()
        if(!is.null(invr)) {
                message("getting cached data")
                return(invr)
        }
        data <- x$get()
        invr <- solve(data, ...)
        x$setinverse(invr)
        invr
}
