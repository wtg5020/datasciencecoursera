##The functions below take a matrix as an input and save it, then it takes the inverse of
##that matrix and prints it out from the cache, if one doesnt exist in the cache then
## it calcuates the inverse of that matrix. 

## This function takes a matrix and caches it

makeCacheMatrix <- function(x = matrix()) {

  
inverseMatrix <- NULL

setMatrix <- function(y) {
  x <<- y
  inverseMatrix <<- NULL
}
getMatrix <- function() x
setInvMatrix <- function(input) inverseMatrix <<- input
getInvMatrix <- function() inverseMatrix

list <- list(setMat = setMatrix, getMat = getMatrix, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)

}
## This calls the cached matrix and checks to see if it exists, if so it returns the cached inverse and prints, if not it 
## calculates the inverse and it prints it

cacheSolve <- function(x, ...) {
       
  inverseMatrix <- x$getInvMatrix()
  if (!is.null(inverseMatrix)) {
    print("fetching cached matrix")
    return(inverseMatrix)
   
  }
  matrix <- x$getMat()

  inverseMatrix <- solve(matrix, ...)
  x$setInvMatrix(inverseMatrix)
  inverseMatrix
  

}

matrix <- matrix(1:4, 2)
cachedMat <- makeCacheMatrix(matrix)
result <- cacheSolve(cachedMat)
print(result)