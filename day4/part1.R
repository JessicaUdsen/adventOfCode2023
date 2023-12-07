library(tidyverse)
#Read problem statement here: https://adventofcode.com/2023/day/4
input <- read_lines('day4FinalData.txt')


cards <- mapply(function(line){
  cardNum <- line %>% str_split(': ') %>% unlist() %>% .[[1]] %>% str_extract('\\d+') %>% strtoi()
  numbers <- line %>% str_split(': ') %>% unlist() %>% .[[2]] %>% str_split(' \\| ') %>% unlist()
  winningVect <- numbers[[1]] %>% str_extract_all('\\d+') %>% unlist()
  ticketVect <- numbers[[2]] %>% str_extract_all('\\d+') %>% unlist()
  intersection <- intersect(winningVect, ticketVect)

  cardTibble <- tibble(
    cardNum = cardNum,
    winningStr = numbers[[1]],
    ticketStr = numbers[[2]],
    intersectionStr = toString(intersection),
    numWinners = length(intersection),
    value = if(numWinners > 0) {2 ** (numWinners - 1)} else{0}
  )
  
  return(cardTibble)
}, input, SIMPLIFY = FALSE) %>%
  bind_rows()

answer <- sum(cards$value)
answer

