input <- read_csv("input-2021-8.txt", col_names = F)

parsed <- input %>% separate(X1, c("pattern", "output"), sep=" \\| ") %>% mutate(entry = row_number())

# Part 1
parsed %>% separate_rows(output) %>% summarize(solution = sum(str_length(output) %in% c(2, 3, 4, 7)))

# Digits have a unique "fingerprint" of how many sides they share with others
# Use a self-join and a count to measure this

fingerprint <- function(d, key) {
  spl <- d %>%
  mutate(letter = str_split(pattern, "")) %>% unnest(letter)
  
  spl %>%
  inner_join(spl, by = c("letter", group_vars(d)), suffix = c("", "2")) %>%
    count({{ key }}, letter) %>%
    group_by({{ key }}, .add = TRUE) %>%
    summarize(fingerprint = paste0(sort(n), collapse = "/")) }


fingerprints <- c("abcefg", "cf", "acdeg", "acdgf", "bcdf", "abdfg",
"abdefg", "acf", "abcdefg", "abcdfg") %>%
  tibble::enframe("digit", "pattern") %>% mutate (digit = digit - 1) %>% fingerprint(digit)


# The output letters can be in any order
sort_letters <- function(s) {
map_chr(map(str_split(s, ""), sort), paste, collapse = "") }

# Now we can match (entry, pattern) -> value

mapping <- parsed %>% separate_rows(pattern) %>% mutate(pattern = sort_letters (pattern)) %>% group_by(entry) %>% fingerprint(pattern) %>% inner_join(fingerprints, by = "fingerprint")

# Match those to the outputs, and combine the digits to get the solution
parsed %>%
separate_rows(output) %>%
  mutate(output = sort_letters(output)) %>%
  inner_join(mapping, by = c("entry", "output" = "pattern")) %>% group_by(entry) %>% summarize(value = as.integer(paste(digit, collapse = ""))) %>% summarize(sum(value))
