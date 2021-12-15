library(tidyverse)
input <- read_csv("input14.txt", col_names = F)

start <- input$X1[1]
# Each pair becomes two new pairs
rules <- input %>%
slice(-1) %>% separate(X1, c("pair", "insert"), sep= " -> ") %>%
  mutate(pair1 = paste0(str_sub(pair, 1, 1), insert),
         pair2 = paste0(insert, str_sub(pair, 2, 2)))

# Create a transition matrix with two 1s in each row
pairs <- unique(c(rules$pair, rules$pair1, rules$pair2))
m <- matrix(0, length(pairs), length(pairs), dimnames = list(pairs, pairs))
m[cbind(rules$pair, rules$pair1)] <- 1
m[cbind(rules$pair, rules$pair2)] <- m[cbind(rules$pair, rules$pair2)] + 1


# Apply the transition matrix 40 for 40 steps
library(expm)
start_counts <- str_count(start, pairs)

frequencies <- start_counts %*% (m %^% 10)

# Count the second letter of each pair
tibble(n = c(frequencies),
pair = colnames(frequencies)) %>%
  group_by(second = str_sub(pair, 2, 2)) %>%
  summarize(n = sum(n)) %>% summarize(solution = diff(range(n)))
