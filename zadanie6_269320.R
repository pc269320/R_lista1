# autor: Patryk Czopek 269320

# Function returns the complementary DNA strand for the given oligonucleotide
# (sense strand of DNA).
# Parameters:
# - nucleotides (String) - string representing the sense strand of DNA
# composed of nucleotides A, T, C, G
# Returns:
# - complementList (String) - string representing the complementary DNA strand
# Exceptions:
# - if any nucleotide is not 'A', 'T', 'C', or 'G'
komplement <- function(nucleotides){
  
  # sprawdzenie czy nie podano pustej sekwencji
  if (nchar(nucleotides) == 0){
    stop("Sequence cannot be empty.")
  }
  
  # inicjalizacja komplementacji
  complementList <- character()
  
  for (i in 1:nchar(nucleotides)){
    nt <- substr(nucleotides, i, i)
    complementNucleotide <- switch(nt,
                                   "A" = "T",
                                   "T" = "A",
                                   "C" = "G",
                                   "G" = "C",
                                   stop(paste("Invalid nucleotide:", nt))
    )
    complementList <- c(complementList, complementNucleotide)
  }
  return(paste0(complementList, collapse = ""))
}

# Function returns the messenger RNA (mRNA) sequence for the given oligonucleotide
# (antisense strand of DNA).
# Parameters:
# - nucleotides (String) - string representing the antisense strand of DNA
# composed of nucleotides A, T, C, G
# Returns:
# - rnaList (String) - string representing the mRNA sequence
# Exceptions:
# - if any nucleotide is not 'A', 'T', 'C', or 'G'
transkrybuj <- function(nucleotides){
  
  # sprawdzenie czy nie podano pustej sekwencji
  if (nchar(nucleotides) == 0){
    stop("Sequence cannot be empty.")
  }
  
  # inicjalizacja transkrybcji
  rnaList <- character()
  
  for (i in 1:nchar(nucleotides)){
    nt <- substr(nucleotides, i, i)
    rnaNucleotide <- switch(nt,
                            "A" = "U",
                            "T" = "A",
                            "C" = "G",
                            "G" = "C",
                            stop(paste("Invalid nucleotide:", nt))
    )
    rnaList <- c(rnaList, rnaNucleotide)
  }
  return(paste0(rnaList, collapse = ""))
}

# testy
test_all <- function(){

  sequence <- "ATCGAAAGCT"
  cat("Sense strand:", sequence, "\n")
  cat("Complementary DNA:", komplement(sequence), "\n")
  cat("Messenger RNA:", transkrybuj(komplement(sequence)), "\n\n")
  
  sequence <- "ATCG"
  stopifnot(komplement(sequence) == "TAGC")
  stopifnot(transkrybuj(komplement(sequence)) == "AUCG")
  
  cat("\nInvalid sequence 'AUCGU'.\n")
  tryCatch({
    komplement("AUCGU")
  }, error = function(e){
    cat(e$message, "\n")
  })
  
  cat("\nInvalid sequence 'ABC'.\n")
  tryCatch({
    komplement("ABC")
  }, error = function(e){
    cat(e$message, "\n")
  })
  
  # puste stringi dla obu funkcji
  cat("\nEmpty sequence for komplement.\n")
  tryCatch({
    komplement("")
  }, error = function(e) {
    cat(e$message, "\n")
  })
  
  cat("\nEmpty sequence for transkrybuj.\n")
  tryCatch({
    transkrybuj("")
  }, error = function(e) {
    cat(e$message, "\n")
  })
}

test_all()