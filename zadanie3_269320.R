# autor: Patryk Czopek 269320

# Function returns all possible subsets of the given set.
# Parameters:
# - xSet (vector) - input vector representing the set
# Returns:
# - subsets (list of vectors) - list containing all subsets of xSet
# Exceptions:
# - if xSet is empty

podzbiory <- function(xSet){
  
  # sprawdzenie czy lista nie jest pusta
  if (length(xSet) == 0){
    stop("List cannot be empty.")
  }
  
  # puste podzbiory
  subsets <- list(list())
  
  for (element in xSet){
    size <- length(subsets)
    for (i in 1:size){
      newSubset <- c(subsets[[i]], element)
      subsets <- c(subsets, list(newSubset))
    }
  }
  
  # weryfikacja ilości podzbiorów
  expectedCount <- 2^length(xSet)
  actualCount <- length(subsets)

  if (actualCount != expectedCount){
    stop(sprintf("Got %d subsets, expected %d", actualCount, expectedCount))
  }
  return(subsets)
}

# testy
test_podzbiory <- function(){

  result <- podzbiory(c("a", "b", "c", "d"))
  cat("Number of subsets for [a, b, c, d]:", length(result), "\n")  # oczekiwane 16

  result <- podzbiory(c("a", "b", "b"))
  cat("Number of subsets for [a, b, b]:", length(result), "\n")  # oczekiwane 8
  
  result <- podzbiory(c("a"))
  cat("Number of subsets for [a]:", length(result), "\n")  # oczekiwane 2
  
  cat("\nEmpty set was given as input.\n")
  tryCatch({
    podzbiory(character())
  }, error = function(e){
    cat(e$message)
  })
}

test_podzbiory()