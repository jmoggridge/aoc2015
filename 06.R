#--- Day 6: Probably a Fire Hazard ------------------------------------
#
# Because your neighbors keep defeating you in the holiday house decorating
# contest year after year, you've decided to deploy one million lights in a
# 1000x1000 grid.
#
# Furthermore, because you've been especially nice this year, Santa has mailed
# you instructions on how to display the ideal lighting configuration.
#
# Lights in your grid are numbered from 0 to 999 in each direction; the lights
# at each corner are at 0,0, 0,999, 999,999, and 999,0.
#
# The instructions include whether to turn on, turn off, or toggle various
# inclusive ranges given as coordinate pairs.
#
# Each coordinate pair represents opposite corners of a rectangle, inclusive; a
# coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3
# square. The lights all start turned off.
#
# To defeat your neighbors this year, all you have to do is set up your lights
# by doing the instructions Santa sent you in order.
#
# For example:
#
# turn on 0,0 through 999,999 would turn on (or leave on) every light. toggle
# 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the
# ones that were on, and turning on the ones that were off. turn off 499,499
# through 500,500 would turn off (or leave off) the middle four lights. After
# following the instructions, how many lights are lit?
#
#
library(tidyverse)

init_lights <- expand_grid(x = 0:999, y = x) |>
  mutate(on = FALSE)

steps <-
  tibble(step = read_lines('input06.txt')) |>
  mutate(
    action = str_extract(step, 'toggle|turn off|turn on'),
    x = str_extract_all(step, '[\\d]+,'),
    y = str_extract_all(step, ',[\\d]+'),
  ) |>
  mutate(
    x = map(x, ~set_names(., c('x1', 'x2'))),
    y = map(y, ~set_names(., c('y1', 'y2')))
  ) |>
  unnest_wider(c(x,y)) |>
  mutate(across(x1:y2, ~.x |> str_remove(',') |> as.numeric())) |>
  mutate(
    lights = pmap(
      .l = list(x1, x2, y1, y2),
      .f = function(x1, x2, y1, y2) expand_grid(x = x1:x2, y = y1:y2)
    )
  ) |>
  select(action, lights)



update_lights <- function(lights, targets, action){
  updated_targets <-
    targets |>
    left_join(lights, by = c('x', 'y')) |>
    mutate(on = case_when(
      action == 'turn on' ~ TRUE,
      action == 'turn off' ~ FALSE,
      action == 'toggle' ~ !on
    ))
  updated_lights <-
    lights |>
    anti_join(targets, by = c('x', 'y')) |>
    bind_rows(updated_targets)
  return(updated_lights)
}

# tibble(action = 'start', lights = list(lights)) |>
  # unnest(lights) |>


update_lights(init_lights, steps$lights[[1]], action = 'toggle')

state <-  accumulate2(
    .x = steps$lights,
    .y = steps$action,
    .f = function(lights, targets, action) {
      update_lights(lights, targets, action)
    },
    .init = init_lights
  ) |>
  last() |>
  pull(on) |>
  sum()





# Part 2 ---------------------------------------------------------------

update_lights2 <- function(lights, targets, action){
  updated_targets <-
    targets |>
    left_join(lights, by = c('x', 'y')) |>
    mutate(
      brightness = case_when(
        action == 'toggle' ~ brightness + 2,
        action == 'turn on' ~ brightness + 1,
        action == 'turn off' ~ brightness - 1,
      ),
      brightness = if_else(brightness < 0, 0, brightness)
    )

  lights |>
    anti_join(targets, by = c('x', 'y')) |>
    bind_rows(updated_targets)
}

init_lights2 <- expand_grid(x = 0:999, y = x) |>
  mutate(brightness = 0)

total_brightness <-
  accumulate2(
    .x = steps$lights,
    .y = steps$action,
    .f = function(lights, targets, action) {
      update_lights2(lights, targets, action)
    },
    .init = init_lights2
  ) |>
  last() |>
  pull(brightness) |>
  sum()


total_brightness










