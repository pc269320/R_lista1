# autor: Patryk Czopek 269320

# Function returns the common elements of two lists.
# Parameters:
# - xList (vector) - first input list
# - yList (vector) - second input list
# Returns:
# - commonElements (vector) - common elements found in both lists (including duplicates)
# Exceptions:
# - if either of the lists is empty
wspolne <- function(xList, yList){
  
  #sprawdzenie czy listy nie są puste
  if (length(xList) == 0 || length(yList) == 0){
    stop("Lists cannot be empty.")
  }
  
  #utworzenie pustej listy i skopiowanie yList
  commonElements <- c()
  yListCopy <- yList
  
  #sprawdzanie czy element w liście X występuje w kopii listy Y
  for (element in xList){
    matchIndex <- match(element, yListCopy)
    if (!is.na(matchIndex)){
      commonElements <- c(commonElements, element)
      yListCopy <- yListCopy[-matchIndex]
    }
  }
  return(commonElements)
}

# testy funkcji
test_wspolne <- function(){
  result <- wspolne(c(1, 2, 2, 3), c(2, 2, 5))
  stopifnot(identical(sort(result), sort(c(2, 2))))
  
  result <- wspolne(c(1, 1, 1, 1), c(5, 5, 5))
  stopifnot(length(result) == 0)
  
  result <- wspolne(c(1, 3, 2, 1), c(2, 1, 1, 3))
  stopifnot(identical(sort(result), sort(c(1, 1, 2, 3))))
  
  result <- wspolne(c(1, 1, 1), c(1, 1, 1, 1))
  stopifnot(identical(sort(result), sort(c(1, 1, 1))))
  
  message("\nSuccessfully passed assertions.")
  
  message("\nOne of the lists given as an argument is empty.")
  tryCatch({
    wspolne(c(), c(1, 2, 3))
  }, error = function(e) {
    message(e$message, "\n")
  })
}

test_wspolne()