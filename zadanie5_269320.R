# autor: Patryk Czopek 269320

# Function calculates the Collatz sequence for given starting integer c0.
# Parameters:
# - c0 (Int) - starting integer for the Collatz sequence
# Returns:
# - sequence (Int vector) - Collatz sequence
# Exceptions:
# - if c0 <= 0
collatzSequence <- function(c0){
  
  # sprawdzenie warunków początkowych
  if (c0 <= 0){
    stop("Parameter c0 has to be a positive integer.")
  }
  
  # inicjalizacja ciągu
  sequence <- c(c0)
  current <- c0
  maxValue <- c0
  
  while (current != 1){
    if (current %% 2 == 0){
      current <- current / 2
    } else{
      current <- 3 * current + 1
    }
    sequence <- c(sequence, current)
    if (current > maxValue){
      maxValue <- current
    }
  }
  
  cat("Maximum value of the sequence for starting point c0 =", c0, "is", maxValue)
  cat("\nSequence length for starting point c0 =", c0, "is", length(sequence))
  return(sequence)
}

# testy
testy <- function(){
  sequence <- collatzSequence(5)
  cat("\nCollatz sequence for c0=5:", sequence, "\n\n")
  
  sequence <- collatzSequence(27)
  cat("\nCollatz sequence for c0=27:", sequence, "\n\n")
  
  sequence <- collatzSequence(4)
  stopifnot(sequence == c(4, 2, 1))
  cat("\n\n")
  sequence <- collatzSequence(6)
  stopifnot(sequence == c(6, 3, 10, 5, 16, 8, 4, 2, 1))
  
  cat("\n\nValue of c0 is zero.\n")
  tryCatch({
    sequence <- collatzSequence(0)
  }, error = function(e){
    cat(e$message, "\n")
  })
}

testy()