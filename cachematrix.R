
      ## The following two functions compute and cache the inverse of a matrix

      ## The first function makeCacheMatrix creates a special object 
      ## that can cache its inverse by the following
      ## 1. set the matrix
      ## 2. get the matrix
      ## 3. set the inverse of the matrix
      ## 4. get the inverse of the matrix

      makeCacheMatrix <- function(x = matrix()) {
            minv <- NULL
            set <- function(y) {
            x <<- y
            minv <<- NULL
            }
            get <- function() x
            set_minv <- function(inverse) minv <<- inverse
            get_minv <- function() minv
                  list(set = set, get = get, setminv = setminv, getminv = getminv)
      }
      
      ## The following function computes the inverse
      ## of the special object ("matrix") returned by makeCacheMatrix.
      ## If the inverse has already been calculated, with no change to matrix,
      ## then the cache solve retrieves the inverse from the cache.
      
      cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
            minv = x$getminv()
            ## If we have the inverse calculated already,
            ## it gets the matrix from the cache without further computations
            
            if(!is.null(minv)){
            message("getting cached data")
            return(minv)
            }
            ## In other cases, the inverse is calculated
        
            matrix <- x$get()
            minv <- solve(matrix, ...)
            x$setminv(minv)
             return(minv)
       }
        



