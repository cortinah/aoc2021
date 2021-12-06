## Part 1
fish <- as.numeric(strsplit(readLines("input-2021-6.txt"), ",")[[1]])
fish <- table(fish)
count <- numeric(9L)
count[2:6] <- fish

gen <- function(school) {
  count <- numeric(9L)
  count[9] <- school[1]
  count[8] <- school[9]
  count[7] <- school[8]+school[1]
  count[1:6] <- school[2:7]
  count
}

for (i in 1:80) count <- gen(count) 
sum(count)

## Part 2
fish <- as.numeric(strsplit(readLines("input-2021-6.txt"), ",")[[1]])
fish <- table(fish)
count <- numeric(9L)
count[2:6] <- fish

gen <- function(school) {
  count <- numeric(9L)
  count[9] <- school[1]
  count[8] <- school[9]
  count[7] <- school[8]+school[1]
  count[1:6] <- school[2:7]
  count
}

for (i in 1:256) count <- gen(count) 
options(scipen=99)
sum(count)