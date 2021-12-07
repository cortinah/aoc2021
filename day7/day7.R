## Part 1
crabs <- as.numeric(strsplit(readLines("input-2021-7.txt"), ",")[[1]])

fuel <- function(crabs, pos)  sum(abs(crabs-pos))
gas <- numeric(length(crabs))
for (i in min(crabs):max(crabs)) gas[i] <- fuel(crabs, i)
min(gas)


## Part 2

fuel <- function(crabs, pos) sum(abs(pos-crabs)*(1+abs(pos-crabs))/2)
gas <- numeric(length(crabs))
for (i in min(crabs):max(crabs)) gas[i] <- fuel(crabs, i)
min(gas)
