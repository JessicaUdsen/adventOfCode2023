library(tidyverse)

input <- read_lines('day3FinalInput.txt')
#Read problem statement here: https://adventofcode.com/2023/day/3
#This is again heavily inspired by Naturage on the AoC Subreddit.
#the from and to columns produced give the ranges for neighbors of these numbers.
#Things I learned here: str_locate_all, between
numbers <- mapply(function(line, row){
  numberTibble <- tibble(
    number = str_match_all(line, '\\d+') %>% unlist() %>% as.numeric(),
    numRow = row,
    colStart = str_locate_all(line, '\\d+') %>% .[[1]] %>% as.matrix() %>% .[, 'start'],
    colEnd = str_locate_all(line, '\\d+') %>% .[[1]] %>% as.matrix() %>% .[,'end']
  ) %>%
    mutate(fromColNum = colStart-1, toColNum = colEnd + 1,
           fromRowNum = row-1  , toRowNum = row + 1)
}, input, row = 1:length(input), SIMPLIFY = FALSE) %>% bind_rows()

symbols <- mapply(function(line, row){
  symbolTibble <- tibble(
    symbol = str_match_all(line, '\\*') %>% unlist(),
    symRow = row, 
    symCol = str_locate_all(line, '\\*') %>% .[[1]] %>% as.matrix() %>% .[, 'start']
  )
}, input, row = 1:length(input), SIMPLIFY = FALSE) %>% bind_rows()

symbolNeighbors <- symbols %>%
  inner_join(numbers, by = join_by(between(symRow, fromRowNum, toRowNum),
                                   between(symCol, fromColNum, toColNum))) %>%
  group_by(symRow, symCol) %>%
  mutate(neighborCount = n()) %>%
  filter(neighborCount == 2) %>%
  summarize(ratio = prod(number))

answer <- sum(symbolNeighbors$ratio)