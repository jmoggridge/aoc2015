# --- Day 4: The Ideal Stocking Stuffer ------------------------------------
#
# Santa needs help mining some AdventCoins (very similar to bitcoins) to use as
# gifts for all the economically forward-thinking little girls and boys.
#
# To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
# least five zeroes. The input to the MD5 hash is some secret key (your puzzle
# input, given below) followed by a number in decimal. To mine AdventCoins, you
# must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...)
# that produces such a hash.
#
# For example:
#
# If your secret key is abcdef, the answer is 609043, because the MD5 hash of
# abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest
# such number to do so. If your secret key is pqrstuv, the lowest number it
# combines with to make an MD5 hash starting with five zeroes is 1048970; that
# is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....

# Your puzzle input is yzbqklnj

digest('abcdef609043', algo = 'md5', serialize = F)
digest('pqrstuv1048970', algo = 'md5', serialize = F)

library(digest)
library(dplyr)
library(tidyr)
library(stringr)
library(furrr)
library(future)
plan(multisession)


mine_adventcoin <- function(
    input,
    zeroes = 5,
    start = 0,
    increment = 10^5,
    stop = 10^7){

  regx <- glue::glue('^0a{zeroes}b') |>
    str_replace('a', '{') |>
    str_replace('b', '}')

  while (start < stop) {
    digits <- start:(start + increment)
    md5 <- tibble(
      text = str_glue('{input}{digits}'),
      hash = future_map(text, ~digest(.x, algo = 'md5', serialize = F))
    ) |>
      unnest(hash) |>
      filter(str_detect(hash, regx))
    if (nrow(md5) > 0) break
    start <- start + increment
  }

  md5 |>
    mutate(solutions = str_remove(text, input) |> as.numeric()) |>
    arrange(solutions) |>
    slice_head(n = 1) |>
    pull(solutions)
}

mine_adventcoin('yzbqklnj')
mine_adventcoin('yzbqklnj', zeroes = 6)

