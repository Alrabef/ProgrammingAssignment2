# modified version, forked from rdpeng/ProgrammingAssignment2
# R Programming Week 3 - Loop Functions and Debugging

# The following two functions help in the caching of the inverse of a matrix
# 
# The function makeCacheMatrix as the makeVector returns a list with 
# the result of each function contained with in, and the physical local in which those
# results are stored in the memory. Given the kind of assignation 
# for the variables in makeCacheMatrix function, it is made a copy to the parent
# envirmonment. Then the function and the values of the variables can be accessed
# from the entire environment. 
# 
# As descrived in (https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md)
# 
# function maKeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL                                             # initializes the inverse to NULL and x as a matrix
        set.matrix <- function(y) {                                 # this function put inverse and the matrix x in the cache
                x <<- y
                inverse <<- NULL
        }
        
        get.matrix <- function() x                                  # this function gets de matrix stored in x
        setinverse.matrix <- function(inverse.matrix) 
                inverse <<- inverse.matrix                          # set the inverse of the matrix putting this value into the location of inverse
        getinverse.matrix <- function() inverse                     # get the value of the inverse of the matrix stored in inverse
        list(set.matrix = set.matrix, get.matrix = get.matrix,      # vector containing the result of the operations, retrieving the values stored 
                                                                    # in the cache
             setinverse.matrix = setinverse.matrix,
             getinverse.matrix = getinverse.matrix)	
}


# function cacheSolve

# This function solves the problem of the matrix stored in the cache which were introduced in the nakeCacheMatrix function

cacheSolve <- function(x, ...){
        inverse <- x$getinverse.matrix()            # Extracts the part of the vector which contains the inverse variable, value stored in cache
        if(!is.null(inverse)) {
                message("Get the matrix in cache")    # if the variable inverse is not(NULL) shows a "message" and the value in inverse
                return(inverse)
        }
        data <- x$get.matrix()
        inverse.matrix <- solve(data, ...)           # calculates the inverse of the matrix stored in data...
        x$setinverse.matrix(inverse.matrix)          # take the inverse stored
        inverse.matrix                               # return the inverse of the matrix.
}

# Ouput examples:

#> m <-makeCacheMatrix()
#> m$set.matrix(matrix(c(1,2,3,2),2,2))
#> m$get.matrix()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    2
#> cacheSolve(m)
#     [,1]  [,2]
#[1,] -0.5  0.75
#[2,]  0.5 -0.25
#> cacheSolve(m)
#Get the matrix in cache
#     [,1]  [,2]
#[1,] -0.5  0.75
#[2,]  0.5 -0.25

#> m <-makeCacheMatrix()
#> m$set.matrix(matrix(c(0,1,1,0),2,2))
#> m$get.matrix()
#     [,1] [,2]
#[1,]    0    1
#[2,]    1    0
#> cacheSolve(m)
#     [,1] [,2]
#[1,]    0    1
#[2,]    1    0
#> cacheSolve(m)
#Get the matrix in cache
#     [,1] [,2]
#[1,]    0    1
#[2,]    1    0
# In the last example, the reason is the matrix(c(0,1,1,0),2,2) is its own inverse.