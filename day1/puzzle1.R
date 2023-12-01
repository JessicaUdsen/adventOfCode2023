library(tidyverse)

readInput <- function(file){
  return(read_delim(file, delim = "\\n", col_names = FALSE))
}

getCoords <- function(numString){
  firstDigit <- str_sub(numString, 1, 1)
  lastDigit <- str_sub(numString, nchar(numString), nchar(numString))
  coords <- paste0(firstDigit, lastDigit)
  coordsInt <- strtoi(coords)
  return(coordsInt)
}

inputFile <- 'inputPuzzle1.txt'
input <- readInput(inputFile)

cleanInput <- input %>%
  mutate(digits = str_replace_all(X1, "[^[:digit:]]", "")) %>%
  mutate(coords = getCoords(digits))
#cleanInput

total <- sum(cleanInput$coords)
total

