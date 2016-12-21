## This set of functions implements a matrix cacheing concept to allow speed up 
## multiple matrix inversions of the same matrix by storing the result inside the 
## wrapper function makeCacheMatrix
## this group of functions currently only work for a square matrix using the Solve function
##
## USAGE:  create a new CacheMatrix by calling makeCacheMatrix with the matrix to be Solved
## myCacheMatrix<-makeCacheMatrix(myMatrix)
##
## Now, each time a "Solved" version is required call cacheSolve with the CacheMatrix created above
## mySolvedMatrix<-cacheSolve(myCacheMatrix)
##
##
## makeCacheMatrix returns a list of function calls that implement the following:
## get - gets the non-inverted matrix
## set - stores the non-inverted matrix
## setinv - stores the inverted matrix
## getinv - gets the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_m <- NULL
        set <- function(y) {
                x <<- y
                inv_m <<- NULL
        }
        get <- function() x
        setinv <- function(m_i) inv_m <<- m_i
        getinv <- function() inv_m
        list(set=set, 
             get=get, 
             setinv=setinv,
             getinv=getinv)
}


## cacheSolve "Solves" the matrix by performing a matrix inversion or returning a copy
## if the matrix has already been solved
## returns a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_i <- x$getinv()
        if(!is.null(m_i)) {
                message("getting cached inverse")
                return(m_i)
        }
        m_matrix <- x$get()
        m_i <- solve(m_matrix)
        x$setinv(m_i)
        m_i
}
