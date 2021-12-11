library(tidyverse)
floor_map <- read_table("input-2021-9.txt", col_names = F)
floor_map <- as.numeric(str_split(floor_map,'')[[1]])
floor_map <- matrix(floor_map[!is.na(floor_map)], ncol=100, byrow = T)

# Part 1
risk_level <- function(d) {
  d_eu <- rbind(d[-1, , drop = FALSE], 10)
  d_ed <- rbind(10, d[-nrow(d), , drop = FALSE])
  d_le <- cbind(d[ , -1, drop = FALSE], 10)
  d_re <- cbind(10, d[ , -ncol(d), drop = FALSE])
  lows <- (d<d_eu) & (d<d_ed) & (d<d_le) & (d<d_re)
  sum(lows*(d+1))
}
risk_level(floor_map)

# Part 2
lows_list <- function(d) {
d_eu <- rbind(d[-1, , drop = FALSE], 10)
d_ed <- rbind(10, d[-nrow(d), , drop = FALSE])
d_le <- cbind(d[ , -1, drop = FALSE], 10)
d_re <- cbind(10, d[ , -ncol(d), drop = FALSE])
lows <- (d<d_eu) & (d<d_ed) & (d<d_le) & (d<d_re)
lows<-data.frame(lows)
lows$Y<-1:nrow(lows)
colnames(lows)<-c(1:ncol(d),"Y")
pivot_longer(lows,1:ncol(d)) %>% filter(value==TRUE) %>% rename(X=name) %>% select(2:1) -> lows_list
lows_list %>% mutate_all(as.numeric)
}


pad_map <- function(d){
  
  padded <- matrix(10, ncol=ncol(d)+2, nrow=nrow(d)+2)
  
  padded[2:(nrow(d)+1), 2:(ncol(d)+1)]=d
  return(padded)
}


find_basin <- function(Y, X) {
  
  if(.GlobalEnv$basins[Y,X]!=0) return()
  if(.GlobalEnv$padded[Y,X]<9) (.GlobalEnv$basins[Y,X] <- 1) else{.GlobalEnv$basins[Y,X] <- -1;
                                                                     return()}
find_basin((Y-1), X)
find_basin((Y+1), X)
find_basin(Y, (X+1))
find_basin(Y, (X-1))
return()
  }

lows <- lows_list(floor_map)
lows <- lows+1
padded <- pad_map(floor_map)

  for (i in 1:nrow(lows)) {
  basins=matrix(0, ncol=ncol(padded), nrow=nrow(padded))
  find_basin(lows[i, 2], lows[i, 1])
  lows[i,3] <- sum(basins==1) }

lows %>% arrange(-V3) %>% select(V3) -> result
prod(as.numeric(result[1:3,1]))
