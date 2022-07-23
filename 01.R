library(tidyverse)

santa1 <-
  readr::read_file('input01.txt') |>
  str_split('') |>
  unlist() |>
  enframe() |>
  mutate(move = if_else(value == '(', 1, -1)) |>
  summarise(solution = sum(move))

read_file('input1.txt') |>
  str_split('') |>
  unlist() |>
  map_dbl(~if_else(.x == '(', 1, -1)) |>
  sum()

santa1b <-
  readr::read_file('input01.txt') |>
  str_split('') |>
  unlist() |>
  enframe() |>
  mutate(move = if_else(value == '(', 1, -1),
         floor = cumsum(move)) |>
  filter(floor == -1) |>
  slice(1) |>
  pull(name)



