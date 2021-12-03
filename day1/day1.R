data <- as.numeric(readLines("input-2021-1.txt"))

# Part 1
sum(diff(data)>0)

# Part 2
library("zoo")
ma <- rollmean(data, 3)
sum(diff(ma)>0)
