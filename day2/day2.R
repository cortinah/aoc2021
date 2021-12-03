data <- read.csv("input-2021-2.txt", sep = " ", header = F)
colnames(data) <- c('dir', 'amt')
library(dplyr)

## Part 1
agg <- data %>% group_by(dir) %>% summarize(sum(amt))
(agg[1,2]-agg[3,2])*agg[2,2]
## Part 2

position <- function(data) {
  pos=0; depth=0; aim=0
  for (i in 1:nrow(data)) {
    
    command <- data[i,1]
    amt <- data[i, 2]
    
    if (command=='forward') {pos=pos+amt; depth=depth+(aim*amt)}
    if (command=='up') aim=aim-amt
    if (command=='down') aim=aim+amt
  }
  return(c(pos, depth))
}

result <- position(data)
prod(result)