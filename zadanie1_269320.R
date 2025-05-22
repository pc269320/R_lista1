# autor: Patryk Czopek 269320

# Funkcja Heron oblicza pole trójkąta ze wzoru Herona
heron <- function(a, b, c){
  
  # sprawdzenie poprawności długości boków
  if (a <= 0 || b <= 0 || c <= 0){
    stop("Lengths of the triangle's sides have to be over 0.")
  }
  
  if ((a + b <= c) || (a + c <= b) || (b + c <= a)){
    stop("Given side lengths can't be used to create a triangle.")
  }
  
  # obliczenie pola ze wzoru Herona (najpierw obl. połowę obwodu)
  p <- (a + b + c) / 2
  return(sqrt(p * (p - a) * (p - b) * (p - c)))
}

# testy funkcji
test_heron <- function(){
  result <- heron(3, 4, 5)
  stopifnot(result==6.0)
  
  result <- heron(5, 12, 13)
  stopifnot(result==30.0)
  
  result <- heron(5, 5, 6)
  stopifnot(result==12.0)
  
  message("Successfully passed assertion.")

  message("\nTriangle with sides equal to 1.0, 2.0 and 3.0.")
  tryCatch({
    heron(1.0, 2.0, 3.0)
  }, error = function(e) {
    message(e$message)
  })
  
  message("\nTriangle with sides equal to 0.0, 2.0 and 4.0.")
  tryCatch({
    heron(0.0, 2.0, 4.0)
  }, error = function(e) {
    message(e$message)
  })
}

test_heron()