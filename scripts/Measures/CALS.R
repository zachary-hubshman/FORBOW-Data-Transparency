##CALS 
cals_youth <- paste0("cals_",1:20)
cals_parent <- paste0("cals_pr_", 1:20)


FOR <- FOR %>%
  mutate(
    across(
      any_of(c(cals_youth, cals_parent)),
      ~ readr::parse_number(as.character(.x))
    )
  )

FOR <- FOR %>%
  # Recode 9s to NA
  mutate(across(all_of(cals_youth), ~na_if(.x, 9))) %>%
  mutate(across(all_of(cals_parent), ~na_if(.x, 9))) %>%
  mutate(
    cals_y_tot = rowSums(pick(any_of(cals_youth))), 
    cals_p_tot = rowSums(pick(any_of(cals_parent)))
  )
