# ProgrammingAssignment2
R programing: Caching the Inverse of a Matrix
#second programming assignment will require you to write an R function is able to cache potentially time-consuming #computations. For example, taking the mean of a numeric vector is typically a fast operation. However, for a very long 
#vector, it may take too long to compute the mean, especially if it has to be computed repeatedly (e.g. in a loop). If the 
#contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need it again, it can 
#be looked up in the cache rather than recomputed. In this Programming Assignment will take advantage of the scoping rules of 
#the R language and how they can be manipulated to preserve state inside of an R object

#We were given the hints which were "makeVector" and "cachemean". The best way of approaching was adjusting the given information.


makeCacheMatrix <- function(x = matrix()) { 
  inv<- NULL   
          set <- function(y) {
                  x <<- y
                  inv <<- NULL
          }
          get <- function() x
          setinv <- function(inverse) inv <<- inverse 
          getinv = function() inv
          list(set=set, get=get, 
               setinv=setinv, 
               getinv=getinv)
 }


#cacheSolve: 
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
#already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv = x$getinv()
       
        if (!is.null(inv)){        
                message("getting cached data")
                return(inv)
        }
        
        mat.data = x$get()
            inv = solve(mat.data, ...)
            x$setinv(inv)    
            return(inv)
}

