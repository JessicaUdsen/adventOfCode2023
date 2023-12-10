library(tidyverse)
#get problem statement here: https://adventofcode.com/2023/day/5

#As of 10 Dec, I have gotten the data in a nice format, but I haven't really started the actual problem-solving yet

inputStr <- paste(readLines("testInput.txt", warn = FALSE), collapse="\n") %>%
  str_split(pattern = '\\n\\n') %>% unlist()
inputStr[1] <- inputStr[1] %>%
  str_replace('seeds: ', 'seeds:\n') #Insert new line after seeds: so that the format matches
inputList <- sapply(inputStr, function(x) str_split(x, '\n'))

#Clean the list names to correspond to almanac sections and remove text element from 
#each vector
names(inputList) <- sapply(names(inputList), function(x) x <- str_split(x, ':') %>% unlist() %>% .[1])
inputList <- lapply(inputList, function(x) x <- x[2: length(x)])
#separate out seeds info from inputList, inputList will eventually be our almanac
seeds <- inputList$seeds %>% 
  str_split(' ') %>% 
  unlist() %>% 
  strtoi()
inputList$seeds <- NULL

almanac <- lapply(inputList, function(map){
  tibble <- as.tibble(map) %>%
    mutate(destStart = sapply(value, function(x) str_split(x, ' ') %>% unlist() %>% strtoi() %>% .[1]),
           srcStart = sapply(value, function(x) str_split(x, ' ') %>% unlist() %>% strtoi() %>% .[2]),
           range = sapply(value, function(x) str_split(x, ' ') %>% unlist() %>% strtoi() %>% .[3])
           ) %>%
    select(srcStart, destStart, range) %>%
    arrange(srcStart)
  return(tibble)
})