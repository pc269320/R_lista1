# autor: Patryk Czopek 269320

# Function returns the first n numbers of the Fibonacci sequence using a for loop.
# Parameters:
# - n (Int) - number of Fibonacci numbers to generate
# Returns:
# - fibonacciList (vector of Int) - first n Fibonacci numbers
# Exceptions:
# - if n <= 0
fibonacciLoop <- function(n){
  
  # test czy podany argument jest większy niż 0
  if (n <= 0){
    stop("Parameter n has to be a positive integer.")
  }
  
  fibonacciList <- c()
  a <- 0
  b <- 1
  
  for (i in 1:n){
    fibonacciList <- c(fibonacciList, a)
    temp <- a
    a <- b
    b <- temp + b
  }
  
  return(fibonacciList)
}

# Function returns the first n numbers of the Fibonacci sequence using recursion.
# Parameters:
# - n (Int) - number of Fibonacci numbers to generate
# Returns:
# - fibonacciList (vector of Int) - first n Fibonacci numbers
# Exceptions:
# - if n <= 0
fibonacciRecursion <- function(n){
  
  # test czy podany argument jest większy niż 0
  if (n <= 0){
    stop("Parameter n has to be a positive integer.")
  }
  
  fib <- function(n, a, b){
    if (n == 0){
      return(c())
    }
    return(c(a, fib(n - 1, b, a + b)))
  }
  
  return(fib(n, 0, 1))
}

# testy
testy <- function(){
  
  n <- 4
  fibonacciLoopList <- fibonacciLoop(n)
  stopifnot(fibonacciLoopList == c(0, 1, 1, 2))
  
  fibonacciRecursionList <- fibonacciRecursion(n)
  stopifnot(fibonacciRecursionList == c(0, 1, 1, 2))
  
  n <- 10
  fibonacciListLoop <- fibonacciLoop(n)
  cat("Fibonacci sequence using loop for n=", fibonacciListLoop)
  
  fibonacciListRecursion <- fibonacciRecursion(n)
  cat("\nFibonacci sequence using recursion for n=", fibonacciListRecursion)
  
  stopifnot(fibonacciListLoop == fibonacciListRecursion)
  
  cat("\n\nValue of n is zero (loop):\n")
  tryCatch({
    n <- 0
    fibonacciLoop(n)
  }, error = function(e) {
    cat(e$message)
  })
  
  cat("\n\nValue of n is zero (recursion):\n")
  tryCatch({
    n <- 0
    fibonacciRecursion(n)
  }, error = function(e) {
    cat(e$message)
  })
}

testy()