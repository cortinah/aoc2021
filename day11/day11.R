library(tidyverse)
input <- read_table("input-2021-11.txt", col_names = F)
input <- as.numeric(str_split(input,'')[[1]])
input <- matrix(input[!is.na(input)], ncol=10, byrow = T)

# Part 1
boost <- function(d) {
  d_eu <- rbind(d[-1, , drop = FALSE], 0)
  d_ed <- rbind(0, d[-nrow(d), , drop = FALSE])
  d_le <- cbind(d[ , -1, drop = FALSE], 0)
  d_re <- cbind(0, d[ , -ncol(d), drop = FALSE])
  d_lu <- cbind(d_eu[ , -1, drop = FALSE], 0)
  d_ru <- cbind(0, d_eu[ , -ncol(d), drop = FALSE])
  d_ld <- cbind(d_ed[ , -1, drop = FALSE], 0)
  d_rd <- cbind(0, d_ed[ , -ncol(d), drop = FALSE])
 
  d_eu + d_ed + d_le + d_re + d_lu + d_ru + d_ld + d_rd   }

step <- function(board, score=0) {
  booster <- matrix(FALSE, ncol=ncol(board), nrow=nrow(board))
  board <- board+1
   
  repeat {
  tens <- (board>=10 & !booster)
  booster <- booster | (board>=10)
  board <- board + boost(tens)
    
  if(all(boost(tens)==0)) break
        }
  score <- score + sum(board>=10)
  list(score, board*(board<10)) 
}

nsteps <- function(n, board){
  nextstep <- step(board)
  n <- n-1
  while(n>0) {
    nextstep <- step(nextstep[[2]], nextstep[[1]])
    n <- n-1     }
  nextstep
}

nsteps(100, input)

# Part 2
seekstep <- function(board){
  nextstep <- step(board)
  steps <- 1
  repeat {
    nextstep <- step(nextstep[[2]], nextstep[[1]])
    steps <- steps + 1
    if (all(nextstep[[2]]==0)) break     }
  steps }

seekstep(input)
