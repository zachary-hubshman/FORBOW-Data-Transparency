Scared_C_Variables <- paste0("scared_child_", 1:41)
Scared_P_Variables <- paste0("scared_parent_", 1:41)

FOR <- FOR %>%
  mutate(
    across(
      any_of(c(Scared_C_Variables, Scared_P_Variables)),
      ~ readr::parse_number(as.character(.x))
    )
  )

FOR <- FOR %>%
  # Recode 9s to NA
  mutate(across(all_of(Scared_C_Variables), ~na_if(.x, 9))) %>%
  mutate(across(all_of(Scared_P_Variables), ~na_if(.x, 9))) %>%
  
  # Compute total scores
  mutate(
    Scared_C_Total = rowSums(across(all_of(Scared_C_Variables)), na.rm = TRUE),
    Scared_P_Total = rowSums(across(all_of(Scared_P_Variables)), na.rm = TRUE)
  )