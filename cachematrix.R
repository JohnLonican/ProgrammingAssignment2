## makeCacheMatrix takes in a matrix object and then, in order
## 1. initiates a variable i, which will be used to hold the inverse of the matrix
##    this variable is initiated as NULL as the function is creating a new
##    cacheMatrix object, so we cannot just assume that i has a certain value
## 2. set - here I am defining a new function, set, that can be passed a single
##    object, y. The value of x is then updated using the "<<-" operator, and since
##    this function is updating the value of x, any values of i (the inverse of x) will
##    not be relevant... So again I am setting the value of i to NULL.
## 3. get - this is just a function that will return the current value of x
## 4. set_inverse - since the function makeCacheMatrix does not itself calculate the
##    inverse of the matrix x, the function set_inverse can be used to update the value of
##    i with the new_inverse of x, passed into the set_inverse function as an argument
## 5. get_inverse - this function simply outputs the current value of i, the inverse.
## 6. list(...) - this is what the function makeCacheMatrix is returning - it is simply a
##    list with 4 items, set, get, set_inverse and get_inverse. These can be used as follows:
##    myMatrix <- matrix(1:4,2,2)
##    myCacheMatrix <- makeCacheMatrix(myMatrix)
##    myCacheMatrix$get - returns myMatrix
##    myCacheMatrix$set(newMatrix) - lets me set the matrix in myCacheMatrix to another matrix
##    myCacheMatrix$get_inverse - returns the inverse of the matrix contained in myCacheMatrix
##    myCacheMatrix$set_inverse(newInverse) - lets me set the inverse of the containing matrix to newInverse


makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL # (1)
  
  set <- function(y) { # (2)
    x <<- y
    i <<- NULL
  }
  
  get <- function() { # (3)
    x
  }
  
  set_inverse <- function(new_inverse) { # (4)
    i <<- new_inverse
  }
  
  get_inverse <- function() { # (5)
    i
  }
  
  list(set = set, get = get, # (6)
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## cacheSolve basically returns the inverse of the matrix stored in the CacheMatrix object x
## it performs the following, in order:
## 1. check the get_inverse of the CacheMatrix object x, assigning the result into variable i
## 2. check to see if the new value of i is null (i.e. it has no inverse value)... If it is not null
##    it will output a message telling the user that it is looking up the cached inverse and
##    then it will return the value of i (the inverse that was pulled from the CacheMatrix object)
##
##    If the value of i was NULL, then the inverse still needs to be calculated, and so the following is done:
## 3. use the get function of the CacheMatrix to get the matrix to inverse
## 4. use the solve() function to calculate the inverse of the matrix_to_inverse and pass the resulting inverse
##    to the variable inverse
## 5. use the set_inverse method of the CacheMatrix object to set the inverse of that object to the value stored in inverse
## 6. return the value inverse


cacheSolve <- function(x, ...) {

  i <- x$get_inverse # (1)
  
  if (!is.null(i)) { # (2)
    message("looking up cached inverse...")
    return(i)
  }
  else {
    matrix_to_inverse <- x$get # (3)
    inverse <- solve(matrix_to_inverse) # (4)
    x$set_inverse(inverse) # (5)
    inverse # (6)
  }
}
