## This function creates a list that includes 4 member functions:
## set, get, setInv and getInv.
## It uses <<- assignment operator to assign a value to an object in an
## environment that is different from the current environment. 


makeCacheMatrix <- function(x = matrix()) {
    
    # Initializing variable for matrix inversion
    xinv <- NULL    	
    
    # set function is used to set a matrix to object created by 
    # makeCacheMatrix function  
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    
    # This function returns the input matrix 
    get <- function() {
        x
    }
    
    # This function set the inversed matrix
    setInv <- function(inv) {
        xinv <<- inv
    }
    
    # This function returned the inversed matrix
    getInv <- function() {
        xinv
    }
    
    # The below list is returned by makeCacheMatrix
    # and makeCacheMatrix object can be used similar to Class object 
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function uses solve() function to calculate inverse matrix and save the result in cache,
## any successive request for inverse matrix calculation for same matrix data will be retrieved
## from cache

cacheSolve <- function(x, ...) {
    
    # Return a matrix that is the inverse of 'x'
    # Here 'x' is the output of makeCacheMatrix  
    inv = x$getInv()
    
    # If the inverse has been calculated already, get from chched data  
    if (!is.null(inv)) { 
        message("getting from cached data")
        return(inv)
    }
    
    # If not, calculate inverse matrix by using solve()
    matrix_data <- x$get()
    inv <- solve (matrix_data)
    
    # set the value of the inverse matrix in the cache using the setInv function.  
    x$setInv(inv)
    
    # Return the inverse matrix calculation
    message("getting calculated data ...")
    return(inv)
}

#########################################
###### Example of this function #########
#########################################

# source("cachematrix.R")
# m <- makeCacheMatrix(matrix(c(3,2,4,8,9,4,5,2,6), c(3,3)))
# cacheSolve(m)
# getting calculated data ...
#            [,1]        [,2]       [,3]
# [1,] -1.3529412  0.82352941  0.8529412
# [2,]  0.1176471  0.05882353 -0.1176471
# [3,]  0.8235294 -0.58823529 -0.3235294