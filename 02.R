# --- Day 2: I Was Told There Would Be No Math ---

# area for the rectangular prism plus
# the area of the smallest side
"surface_area <-
  2*(l*w) + 2*(w*h) + 2*(h*l) + shortest_side"

# tally all the packages

input_data <-  read_tsv(
  'input02.txt',
  col_names = 'raw',
  show_col_types = F
) |>
  tidyr::separate(
    col = raw,
    into = c('l','h','w'),
    sep = 'x'
  ) |>
  mutate(across(everything(), as.numeric))

wrapping <-
  input_data |>
  transmute(
    lw = l*w*2,
    wh = w*h*2,
    hl = h*l*2,
  ) |>
  rowwise() |>
  mutate(smol = min(c_across(everything())) / 2,
         area = sum(c_across(everything()))) |>
  ungroup() |>
  summarise(square_feet = sum(area)) |>
  print()


ribbon <-
  input_data |>
  mutate(
    perimeter = case_when(
      l <= h & w <= h ~ 2 * (w + l),
      h <= l & w <= l ~ 2 * (h + w),
      h <= w & l <= w ~ 2 * (h + l),
    ),
    bow = l * h * w,
    total = perimeter + bow
    ) |>
  summarise(sum = sum(total)) |>
  print()



