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
    numWinners = length(intersection),
    augMin = if(numWinners == 0){0} else{cardNum + 1},
    augMax = if(numWinners == 0){0} else{cardNum + numWinners},
    copies = 1
  )
  
  return(cardTibble)
}, input, SIMPLIFY = FALSE) %>%
  bind_rows() %>% 
  as.matrix()

for(i in 1:dim(cards)[1]){
  if(cards[i, 'numWinners'] != 0){
    nextIndex <- i + 1
    if(nextIndex < dim(cards)[1]){
      cardsSubset <- cards[nextIndex:dim(cards)[1], ]
  
        for(k in 1:dim(cardsSubset)[1]){
          if(between(cardsSubset[k, 'cardNum'], cards[i, 'augMin'], cards[i, 'augMax'])){
            cardRef <- cardsSubset[k, 'cardNum']
            cards[cardRef, 'copies'] <- cards[cardRef, 'copies'] + cards[i, 'copies']
          }
        }
    }else{
        if(between(cards[nextIndex, 'cardNum'], cards[i, 'augMin'], cards[i, 'augMax'])){
          cards[nextIndex, 'copies'] <- cards[nextIndex, 'copies'] + cards[i, 'copies']
        }
      }
    }
  }


cards <- cards %>% as_tibble()
answer <- sum(cards$copies)


