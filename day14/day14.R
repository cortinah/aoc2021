library(tidyverse)
poly <- read.table("input14.txt", header = F, nrows=1)

rules <- read.table("input14.txt", sep = "-", header = F, skip = 2)
rules %>% separate("V2", into=c('x',"y"),sep = ">") %>% select(-x) -> rules
rules %>% mutate(fm=str_trim(V1)) %>% select(-V1) -> rules
rules %>% mutate(to=paste0(fm, str_trim(y), fm)) %>% select(-y) -> rules

solution <- as.character(poly)

original_count <- str_count(solution, names(deframe(rules)))

rules_list <- deframe(rules)

rules$nxt <- map(rules_list, ~str_count(., names(rules_list)))

rules %>% separate("nxt", into=paste0("c",0:100), convert = T) %>% select(-c0) -> rules

next_count <- original_count

for (i in 1:10) next_count <- colSums(matrix(next_count,ncol=100, nrow=100,byrow = F) * rules[,3:102])


rules %>% select(fm) %>% mutate(count=next_count) -> results

results %>% extract("fm",into=c("a","b"), "([[:alnum:]]+)([[:alnum:]]+)") -> results

pivot_longer(results, cols=1:2) %>% select(value, count) -> results

results %>% group_by(value) %>% summarise(count=sum(count)) -> final


max(final$count) - min(final$count)


str_count( str_replace_all(solution, rules_list), names(rules_list))
