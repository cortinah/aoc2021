library(tidyverse)
data <- read_csv("input-2021-3.csv", col_names = FALSE, trim_ws = FALSE)

## Part 1
sep <- data %>% separate(col=1, sep = '', into=letters[1:13])
sep <- sep[,-1]
colnames(sep) <- letters[1:12]                  

mf <- function(x) names(sort(table(x), decreasing = TRUE)[1])

result=0
for(i in 1:12){
  result[i] <- mf(sep[,i])
}

result <- strtoi(result, base=2)

bitsToInt <- function(x) {
  packBits(rev(c(rep(FALSE, 32-length(x)%%32), as.logical(x))), "integer")
}

bitsToInt(result) * bitsToInt(!result)

## Part 2

mf1 <- function(x){ 
  t <- sort(table(x), decreasing = TRUE)
  if(length(t)==1) return(names(t))
  if(t[1]==t[2]) return("1")
  names(t[1])
  }

sep %>% filter(a==mf1(.[,"a"])) %>% filter(b==mf1(.[,"b"])) %>%
  filter(c==mf1(.[,"c"])) %>% filter(d==mf1(.[,"d"])) %>%
  filter(e==mf1(.[,"e"])) %>% filter(f==mf1(.[,"f"])) %>%
  filter(g==mf1(.[,"g"])) %>% filter(h==mf1(.[,"h"])) %>%
  filter(i==mf1(.[,"i"])) %>% filter(j==mf1(.[,"j"])) %>%
  filter(k==mf1(.[,"k"])) %>% filter(l==mf1(.[,"l"])) -> oxygen

mf0 <- function(x){ 
  t <- sort(table(x), decreasing = TRUE)
  if(length(t)==1) return(names(t))
  if(t[1]==t[2]) return("0")
  names(t[2])
}

sep %>% filter(a==mf0(.[,"a"])) %>% filter(b==mf0(.[,"b"])) %>%
  filter(c==mf0(.[,"c"])) %>% filter(d==mf0(.[,"d"])) %>%
  filter(e==mf0(.[,"e"])) %>% filter(f==mf0(.[,"f"])) %>%
  filter(g==mf0(.[,"g"])) %>% filter(h==mf0(.[,"h"])) %>%
  filter(i==mf0(.[,"i"])) -> scrubber

bitsToInt(strtoi(oxygen)) * bitsToInt(strtoi(scrubber))
