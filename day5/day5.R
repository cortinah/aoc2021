library(tidyverse)
## Part 1
lines <- read.csv("input-2021-5.txt", header = F, sep='>')

lines %>% separate(col = 1,into = c("x1","b"),sep = '-') %>% select(-b) %>%
  separate(col=1,into=c("x1","y1"),sep=',') %>% separate(col='V2',into=c("x2","y2"),sep=',') -> lines

lines %>% mutate_all(as.numeric) -> lines
lines %>% filter(x1==x2 | y1==y2) -> hvlines

board <- matrix(0,ncol=max(lines), nrow=max(lines))

addline <- function(x1,y1,x2,y2, board) {
  board[x1:x2, y1:y2] <- board[x1:x2, y1:y2]+1
  return(board)
}

i=1
repeat {
  board <- addline(hvlines[i,'x1'], hvlines[i,'y1'], hvlines[i,'x2'], hvlines[i,'y2'], board)

  if (i==nrow(hvlines)) break
  i <- i+1  
}

sum(board>=2)

## Part 2
lines %>% filter(x1!=x2 & y1!=y2) -> dlines

adddiag <- function(x1,y1,x2,y2, board) {
  if( ((y2-y1)/(x2-x1)) > 0) {
     
   for(tx in x1:x2) {
        board[tx, y1+tx-x1] <- board[tx, y1+tx-x1]+1
   }
    return(board)
  }
  
  for(tx in x1:x2) {
    board[tx, y1-tx+x1] <- board[tx, y1-tx+x1]+1}
  
  return(board) }

i=1
repeat {
  board <- adddiag(dlines[i,'x1'], dlines[i,'y1'], dlines[i,'x2'],dlines[i,'y2'], board)
  if (i==nrow(dlines)) break
  i <- i+1  
}
sum(board>=2)

#17741
