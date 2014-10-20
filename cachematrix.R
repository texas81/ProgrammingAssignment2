## Put comments here that give an overall description of what your
## CacheMatrix(x) this is a function to cache the inverse of a matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
#  variable initialization, 
    m<-NULL
    # this is the set function to set the matrix, the oiginal matrix
    set<-function(y){
    	x<<-y
    	m<<-NULL
    }
    # this is the get function to get the matrix, the original one
    get<-function() x 
    # this is the function to set the inverse of the matrix
    setmatrix <- function(matrix) m<<-matrix
    # this is the function to set the inverse of the matrix
    getmatrix <- function () m
    # this is the list of manipulation functions
    list(set=set, get=get, 
    setmatrix = setmatrix,
    getmatrix = getmatrix
    )
}


## cacheSolve (x,...) is to calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # if we will have an inverse of the matrix we will get it back
        m<-x$getmatrix()
        if(!is.null(m)){
        	message("getting cached data")
        	return(m)
        }
        # if there is no invese matrix the lets cacluate it
        data<-x$get()
        # the exact inverse caclulation
        m<-solve(data,...)
        x$setmatrix(m)
        m
}
