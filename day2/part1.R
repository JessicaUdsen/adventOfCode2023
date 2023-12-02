#Find problem statement here: https://adventofcode.com/2023/day/2
#Inspired heavily by Naturage on the AoC Subreddit (spec. on the input cleaning)
library(tidyverse)

readInput <- function(file){
  linesVect <- read_lines(file, n_max = -1)
  
  tibbleList <- lapply(linesVect, function(line){
    line %>%
      str_split("(: )|(, )|(; )") %>%
      unlist() %>%
      tibble(game = .[1], color = ., cubes = .)
  })

  tibbleList <- lapply(tibbleList, function(tibble){
    tibble %>%
      mutate(game = str_extract(game, "\\d+"),
             color = str_extract(color, "blue|red|green"),
             cubes = str_extract(cubes, "\\d+") %>% strtoi()) %>%
      slice(-1) %>%
      group_by(game, color) %>%
      summarize(maxCubes = max(cubes))
    }) 

  return(bind_rows(tibbleList))
}

getGoodGames <- function(masterTibble){
  masterTibble <- masterTibble %>%
    pivot_wider(names_from = color, values_from = maxCubes) %>%
    filter(red <= 12 & green <= 13 & blue <= 14)
  return(as.integer(masterTibble$game))
}

input <- readInput('testInputDay2.txt')
goodGames <- getGoodGames(input)
answer <- sum(goodGames)
answer

