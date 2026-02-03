als <- paste0("als_", 1:54)

FOR <- FOR %>%
  mutate(
    across(
      any_of(als),
      ~ readr::parse_number(as.character(.x))
    )
  ) %>%
  mutate(
    across(
      any_of(als),
      ~ dplyr::case_when(
        .x == 3 ~ 0,
        .x == 2 ~ 1,
        .x == 1 ~ 2,
        .x == 0 ~ 3,
        TRUE    ~ NA_real_
      )
    )
  )

FOR <- FOR %>%
  # Recode 9s to NA
  mutate(across(all_of(als), ~na_if(.x, 9))) %>%
  mutate(
    als_total = rowSums(pick(any_of(als)))
  )

