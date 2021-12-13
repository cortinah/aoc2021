library(tidyverse)
input <- read.table("input-13.txt", sep = ",",header = F, nrows = 820)
colnames(input) <- c("x","y")

fold <- function(data, xf=0, yf=0) {
  
  if(xf>0) {data %>% mutate(x=ifelse(x>xf, x-(2*(x-xf)), x)) -> data}
  if(yf>0) {data %>% mutate(y=ifelse(y>yf, y-(2*(y-yf)), y)) -> data}
  
  data %>% distinct(.keep_all = TRUE) }

# Part 1
part1 <- fold(input, xf=655)

# Part 2
part2 <- part1 %>% fold(yf=447) %>% fold(xf=327) %>% fold(yf=223) %>% fold(xf=163) %>%
  fold(yf=111) %>% fold(xf=81) %>% fold(yf=55) %>% fold(xf=40) %>% fold(yf=27) %>% fold(yf=13) %>%
         fold(yf=6)

ggplot(part2, aes(x=x, y=-y)) +geom_point() +theme_void()
