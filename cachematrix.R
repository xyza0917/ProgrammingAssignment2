## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()){   
       verse <- NULL
       set <- function(y){
               x <<- y
               verse <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {verse <<- inverse}
        getInverse <- function() {verse}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        verse <- x$getInverse()
        if(!is.null(verse)){
                message("getting cached data")
                return(verse)
        }
        mat <- x$get()
        verse <- solve(mat, ...)
        x$setInverse(verse)
        verse
}
