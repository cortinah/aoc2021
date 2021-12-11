library(tidyverse)
## Part 1

data <- read_delim("input-2021-8.txt", delim = "|", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)

data %>% select(X2) %>% separate(col = "X2", into = letters[1:4]) %>% pivot_longer(cols = 1:4) %>% select(value) -> data_sum
data_sum %>% mutate(str_l=str_length(value)) %>% group_by(str_l) %>% summarise(str_c=n()) -> data_sum

data_sum %>% filter(str_l==2 | str_l==4 | str_l==3 | str_l==7) -> data_sum
data_sum %>% summarise(sum(str_c))

# length 2 = 1
# length 4 = 4
# length 3 = 7
# length 7 = 8

## Part 2
data %>% separate(col = "X1", into = LETTERS[1:10]) %>% separate(col = "X2", into = letters[1:4]) -> data

decode <- function(string) switch(str_length(string), NA, 1, 7, 4, NA, NA, 8, NA, NA)
decoder <- function(string) map_dbl(string, decode)
data %>% mutate(Da=decoder(a), Db=decoder(b), Dc=decoder(c), Dd=decoder(d))

# 5: 2, 3, 5
# 6: 0, 6 9


