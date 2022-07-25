#' --- Day 5: Doesn't He Have Intern-Elves For This? ------------------------
#' Santa needs help figuring out which strings in his text file are naughty or
#' nice.
#'
#' A nice string is one with all of the following properties:
#'
#' It contains *at least three vowels* (aeiou only), like aei, xazegov, or
#' aeiouaeiouaeiou.
#' It contains *at least one letter that appears twice in a row*,
#' like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
#' It *does not contain* the strings ab, cd, pq, or xy, even if they are part of
#' one of the other requirements. For example:
#'
#' ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...),
#' a double letter (...dd...), and none of the disallowed substrings. aaa is
#' nice because it has at least three vowels and a double letter, even though
#' the letters used by different rules overlap. jchzalrnumimnmhp is naughty
#' because it has no double letter. haegwjzuvuyypxyu is naughty because it
#' contains the string xy. dvszwmarrgswjxmb is naughty because it contains only
#' one vowel. How many strings are nice?
#'
library(tidyverse)

tribble(
  ~word,
  'ugknbfddgicrmopn',
  'aaa',
  'jchzalrnumimnmhp',
  'haegwjzuvuyypxyu',
  'dvszwmarrgswjxmb'
)

count_nice <- function(data) {
  doubles <- glue::glue('{letters}{letters}') |> str_c(collapse = '|')
  not_these <- c('ab','cd','pq','xy') |> str_c(collapse = '|')

  data |>
    mutate(
      vowels = str_count(word, '[aeiou]') >= 3,
      doubles = str_detect(word, doubles),
      not_these = !str_detect(word, not_these),
    ) |>
    rowwise() |>
    mutate(
      nice = all(c_across(c(vowels, doubles, not_these)))
    ) |>
    filter(nice) |>
    nrow()
}

words <-
  read_csv('input05.txt', col_names = 'word', show_col_types = F)

nice <-
  words |>
  count_nice() |>
  print()



##  Part Two ----------------------------------------------------------------

# Now, a nice string is one with all of the following properties:

# - It contains a pair of any two letters that appears at least twice in the
# string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like
# aaa (aa, but it overlaps).



duple_regexp <-
  crossing(a = letters, b = letters) |>
  transmute(pattern = paste0(a, b, '.*?', a, b)) |>
  pull(pattern) |>
  str_c(collapse = '|')



# - It contains at least one letter which repeats with exactly one letter
# between them, like xyx, abcdefeghi (efe), or even aaa.

gapped_regexp <- tibble(a = letters) |>
  transmute(
    pattern = paste0(a, '.', a)
  ) |>
  pull(pattern) |>
  str_c(collapse = '|')


count_nice2 <- function(data, word) {
  data |>
    mutate(
      has_duple = str_detect({{word}}, duple_regexp),
      has_gapped = str_detect({{word}}, gapped_regexp),
    ) |>
    rowwise() |>
    mutate(
      nice = all(c_across(starts_with('has_')))
    )
}

test_words <-
  tribble(
    ~word,
    'qjhvhtzxzqqjkmpb', #nice
    'xxyxx', #nice
    'uurcxstgmygtbstg', #naughty
    'ieodomkazucvgmuy', #naughty
    'aaajxj', # should be false because overlaps
    'bxffbx', # no gapped pattern,
    'bxfxfbx' #  nice
  )

count_nice2(test_words, word = word)

count_nice2(words, word = word) |>
  filter(nice) |>
  nrow()

