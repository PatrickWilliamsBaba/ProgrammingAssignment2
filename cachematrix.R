## ----- Patrick S. Williams -----
## Week3 Peer Review

#----Documentation Section (Do not edit)----
# This program written by Patrick S. Williams using text, code and/or data from:
# R Programming for Data Science, Roger D. Peng, published on 2016-09-29, 
# © 2014 - 2016 Roger D. Peng

## 1. Re-write makeVector() provided in question to cache inverse of x instead of mean (m)

makeCacheMatrix <- function(x = numeric()) {
        inverse_x <- NULL
        set <- function(y) {
                x <<- y
                inverse_x <<- NULL
        }
        get <- function() x      # Below must set matrix rather than mean!
        setinverse_x <- function() inverse_x <<- solve  ## x from environment below!
        getinverse_x <- function() inverse_x
        list(set = set, get = get,
             setinverse_x = setinverse_x,
             getinverse_x = getinverse_x)
}

## 2. Re-write cachmean function in question as cacheSolve to solve for the inverse of
## a matrix.

cacheSolve <- function(x, ...) {  ## Remember, solving for inverse_x!!
        inverse_x <- x$getinverse_x()
        if (!is.null(inverse_x)) {
                message("getting cached data")
                return(inverse_x)
        }
        data_inv_x <- x$get()
        inverse_x <- solve(data_inv_x, ...)
        x$setinverse_x()
        inverse_x
}

## Now, let's test this puppy using a simple matrix, bearing in mind that 
## a * inv_a = I (idenitiy matrix), so we need to define a square
## matrix with the diagonal equal to any number, and other elements as zero.

a <- diag(5,2)  # A square matrix with diagonal=5, 0 elsewhere
a

## With a diagonal of 5, the inverse should have a diagonal of 0.20, with zeros elsewhere,
## such that their product is an identity matrix.

CachedInverse_a <- makeCacheMatrix(a)
inv_a <- cacheSolve(CachedInverse_a)           # Inverse matrix correct

## Show that a * inv_a = I (idenitiy matrix)
I <- a*inv_a
I                                              # Identity matrix is correct
#----------------Program End----------------

