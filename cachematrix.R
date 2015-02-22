## These functions are designed to save computation time in calculating the inverse of a square matrix and 
## returning this inverse matrix from a variable if it has already been calculated 

## Creates a list of 4 functions which allow the user to set and retrieve the matrix to be worked with, or to 
## set and retrieve the inverse of this matrix

makeCacheMatrix <- function (x = matrix()) {
        inv <- NULL	##resets inverse to NULL i.e. has not been calculated yet
        set <- function (y) {
                x <<- y
                inv <<- NULL	##same as above, allows the user to set a new matrix whose inverse will not be returned from cache
        }
        get <- function () x	##returns set matrix
        setInv <- function(inverse) inv <<- inverse	##sets inv variable once inverse is calculated
        getInv <- function () inv
        list(set = set, get = get,	#creates a list of all 4 functions to be called on data set
             setInv = setInv,
             getInv = getInv)
}


## Here, the inverse is calculated if it has not been before, otherwise the inverse it retrieved from  cache

cacheSolve <- function (x, ...) {
        inv <- x$getInv()	#calls getInv() function on matrix, returns inverse matrix
        if (!is.null(inv)) {	#checks value of inv. If it isn't "0" it has been calculated before and it is merely returned using getInv() function
                message ("Getting inverse matrix from cache")
                return(inv)
        }
        matrixData <- x$get() 
        inv <- solve(matrixData) ##else, inverse is calculated with solve() function
        x$setInv(inv)
        inv
