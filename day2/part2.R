#See task description here: https://adventofcode.com/2023/day/2#part2
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

input <- readInput('testInputDay2.txt')
input <- input %>%
  pivot_wider(names_from = color, values_from = maxCubes) %>%
  mutate(minPower = blue * green * red)
answer <- sum(as.integer(input$minPower))
answer
