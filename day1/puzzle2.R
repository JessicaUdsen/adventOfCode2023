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

cleanText <- function(messyText, digitsList){
  cleanedText <- messyText
  for(spelledDigit in names(digitsList)){
    cleanedText <- str_replace_all(cleanedText, spelledDigit, 
                                   digitsList[[spelledDigit]])
  }
  return(cleanedText)
}

#Same input file as for puzzle 1 
inputFile <- 'inputPuzzle1.txt'
input <- readInput(inputFile)
#the idea for the values in this list as subsitution values comes from
#EyedMoon on Reddit in the Advent of Code subreddit. I removed some
#unnecessary letters in the replacements.
#You can't just substitute the spelled numbers with their corresponding digits
#without possibly ruining overlapping spelled out digits. The extra letters in
#the replacement string make up for this, and should work regardless of where
#in the string the digit is spelled out.
#If you want to steal this, remember the replacement strings also depend on 
#the order in which we're searching for these number strings!
digitsList <- list('one' = 'o1e', 'two' = 't2', 'three' = 't3e',
                   'four' = '4', 'five' = '5e', 'six' = '6',
                   'seven' = '7n', 'eight' = 'e8', 'nine' = '9')

cleanInput <- input %>%
  mutate(lowerCase = str_to_lower(X1)) %>%
  mutate(digitSub = cleanText(lowerCase, digitsList)) %>%
  mutate(digitsOnly = str_replace_all(digitSub, "[^[:digit:]]", "")) %>%
  mutate(coords = getCoords(digitsOnly))

total <- sum(cleanInput$coords)
total