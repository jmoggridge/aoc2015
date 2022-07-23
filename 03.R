## --- Day 3: Perfectly Spherical Houses in a Vacuum ---------------------------

#   Santa is delivering presents to an infinite two-dimensional grid of houses.
#
# He begins by delivering a present to the house at his starting location, and then an elf at the North Pole calls him via radio and tells him where to move next
#' `Moves are always exactly one house to the north (^), south (v), east (>), or west (<)`
#' After each move, he delivers another present to the house at his new location.
#' However, the elf back at the north pole has had a little too much eggnog, and so his directions are a little off, and Santa ends up visiting some houses more than once. *How many houses receive at least one present?*
#
#   For example:
#
#   > delivers presents to 2 houses: one at the starting location, and one to the east.
# ^>v< delivers presents to 4 houses in a square, including twice to the house at his starting/ending location.
# ^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.


# How many houses receive at least one present?
# (how many unique locations does santa visit on his path?)
#' not location specific - we can use any system to tally up the total number of houses visited.

#' ^ dx: 0, dy: +1
#' v dx: 0, dy: -1
#' > dx: +1, dy: 0
#' < dx: -1, dy: 0

library(tidyverse)
library(gganimate)

animate_santa <- function(visits){
  plot <-
    visits |>
    mutate(move = row_number()) |>
    ggplot(aes(x, y)) +
    geom_path(aes(color = move), show.legend = FALSE) +
    geom_point(color = 'red', alpha = 0.65, size = 0.52) +
    scale_color_viridis_c() +
    labs(title = '  AOC2015 - day3: Santa delivering presents on an infinite 2d grid',
         subtitle = '    Santa is on move {round(frame*nrow(visits)/100)}') +
    theme_void()

  plot +
    transition_reveal(move) +
    ease_aes('linear')
}


# convert directions to numeric (x,y) moves
path <-
  readLines('input03.txt') |>
  suppressWarnings() |>
  str_split(pattern = '') |>
  unlist() |>
  enframe() |>
  mutate(
    dx = case_when(
      value == '^' ~ 1,
      value == 'v' ~ -1,
      TRUE ~ 0
      ),
    dy = case_when(
      value == '>' ~ 1,
      value == '<' ~ -1,
      TRUE ~ 0
    )
  ) |>
  select(dx, dy)

# santa delivers a present to his starting location (0,0) then follows the path
visits <-
  tibble(dx = 0, dy = 0) |>
  bind_rows(path) |>
  mutate(x = cumsum(dx),
         y = cumsum(dy)) |>
  select(x, y)

# how many unique homes were visited?
visits |>
  distinct() |>
  nrow()



animate_santa(visits)
# anim_save('~/Desktop/santa_path.gif')



## Part 2 ------------------------------------------------------------------

#' The next year, to speed up the process, Santa creates a robot version of himself, Robo-Santa, to deliver presents with him.
#'
#' Santa and Robo-Santa start at the same location (delivering two presents to the same starting house), then take turns moving based on instructions from the elf, who is eggnoggedly reading from the same script as the previous year.
#'
#' This year, how many houses receive at least one present?
#'
#' For example:
#'
#' ^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa goes south.
#' ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up back where they started.
#' ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one direction and Robo-Santa going the other.

move_the_santa <- function(path, position = 1) {
  tibble(
    dx = 0,
    dy = 0
  ) |>
  bind_rows(
    path |> slice(seq(position, nrow(path), 2))
  ) |>
  mutate(
    x = cumsum(dx),
    y = cumsum(dy)
  ) |>
    select(x,y)
}
animate_santas <- function(santa, robo_santa) {
  visits2 <-
    bind_rows(santa |> mutate(santa = 'Santa'),
              robo_santa |> mutate(santa = 'RoboSanta')) |>
    group_by(santa) |>
    mutate(move = row_number()) |>
    ungroup()
  plot <- visits2 |>
    ggplot(aes(x, y)) +
    geom_path(aes(color = santa), alpha = 0.5, show.legend = TRUE) +
    geom_point(aes(color = santa), alpha = 0.65, size = 0.52) +
    rcartocolor::scale_color_carto_d(NULL) +
    labs() +
    theme_void()
  plot +
    transition_reveal(move) +
    ease_aes('linear')
}


santa <- move_the_santa(path)
robo_santa <- move_the_santa(path, position = 2)
together <- bind_rows(santa, robo_santa) |> distinct() |> nrow() |> print()












