library(tidyverse)
## Part 1
numbers <- read.csv("input-2021-4.txt", header = F, nrows = 1)
numbers <- unname(as.list(numbers))

boards <- readChar("input-2021-4.txt", 10000)
boards <- str_split(boards,'\n\n')
boards <- boards[[1]]; boards <- boards[-1]; boards <- as.list(boards)

makematrix <- function(x) {                  
x <- as.numeric(str_split(x,c('\\s'))[[1]])
x <- matrix(x[!is.na(x)], ncol=5) 
x}

boards <- map(boards, makematrix)                  

checkbingo <- function(board, numlist){
  round <- 1
  tests <- matrix(FALSE, ncol=5, nrow=5)
  repeat { 
    tests <- board==numlist[round] | tests
    if (any(rowSums(tests)==5)) return(round)
    if (any(colSums(tests)==5)) return(round)
    if (round==100) return(101)
    round <- round+1
  } }

solution <- which.min(map_dbl(boards, ~checkbingo(., numbers)))

checkscore <- function(board, numlist){
  round <- 1
  tests <- matrix(FALSE, ncol=5, nrow=5)
  repeat { 
    tests <- board==numlist[round] | tests
    if (any(rowSums(tests)==5)) return(sum((!tests)*board*numlist[[round]]))
    if (any(colSums(tests)==5)) return(sum((!tests)*board*numlist[[round]]))
    if (round==100) return(101)
    round <- round+1
  } }

checkscore(boards[[solution]],numbers)

# Part 2
solution <- which.max(map_dbl(boards, ~checkbingo(., numbers)))
checkscore(boards[[solution]], numbers)
