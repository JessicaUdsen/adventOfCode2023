library(tidyverse)
#get problem statement here: https://adventofcode.com/2023/day/5

#As of 10 Dec, I have gotten the data in a nice format, but I haven't really started the actual problem-solving yet

inputStr <- paste(readLines("finalInput.txt", warn = FALSE), collapse="\n") %>%
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
           range = sapply(value, function(x) str_split(x, ' ') %>% unlist() %>% strtoi() %>% .[3]),
           srcEnd = srcStart + range - 1, 
           destEnd = destStart + range - 1
           ) %>%
    select(srcStart, srcEnd, destStart, destEnd, range) %>%
    arrange(srcStart)
  return(tibble)
})

translateFunction <- function(srcVal, mapTibble){
  for(i in 1:dim(mapTibble)[1]){
    if(between(srcVal, mapTibble$srcStart[i], mapTibble$srcEnd[i])){
      srcDiff <- srcVal - mapTibble$srcStart[i]
      return(mapTibble$destStart[i] + srcDiff)
    }
  }
  return(srcVal)
}

seedToSoil <- sapply(seeds, function(seed) translateFunction(seed, almanac[['seed-to-soil map']]))
names(seedToSoil) <- NULL

soilToFertilizer <- sapply(seedToSoil, function(soil) translateFunction(soil, almanac[['soil-to-fertilizer map']])) %>% unlist()
names(soilToFertilizer) <- NULL

fertilizerToWater <- sapply(soilToFertilizer, function(fertilizer) translateFunction(fertilizer, almanac[['fertilizer-to-water map']]))
names(fertilizerToWater) <- NULL

waterToLight <- sapply(fertilizerToWater, function(water) translateFunction(water, almanac[['water-to-light map']]))
names(waterToLight) <- NULL

lightToTemp <- sapply(waterToLight, function(light) translateFunction(light, almanac[['light-to-temperature map']]))
names(lightToTemp) <- NULL

tempToHumidity <- sapply(lightToTemp, function(temp) translateFunction(temp, almanac[['temperature-to-humidity map']]))
names(tempToHumidity) <- NULL

humidityToLocation <- sapply(tempToHumidity, function(humidity) translateFunction(humidity, almanac[['humidity-to-location map']]))
names(humidityToLocation) <- NULL

answer <- min(humidityToLocation)