#Juan Pablo Baron 24/08/2020
# makeCacheMatrix Explanation:
  
#Esta funcion primero pide una matrix y ek codigo la va a invertirT


makeCacheMatrix <- function(x = matrix()) {
  # Guarda un valor en el canche
  # ial inicio esete valor es nulo NULL
  inv <- NULL
  # Crea una matix en el ambiente de trabajo 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Le da valor a la matriz 
  get <- function() x
  # invierte la matrix y la guarda en el cache 
  setinverse <- function(inverse) inv <<- inverse
  # Obtiene la matrix invertida
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#cacheSolve Explanation:
  
# Verificaremos si la funcion esta en el cache
#Si la verificacion es diferente a nulo obtendremos el mensahe "datos en el cache obtenidos"

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("datos en el cache obtenidos")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}


  
#ejemplo

x = rbind(c(3,2,1), c(1,3,2), c(2,1,3))
m = makeCacheMatrix(x)
cacheSolve(m)
