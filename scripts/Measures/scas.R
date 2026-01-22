Scas_C_Variables <- paste0("scas_child_", 1:44)
Scas_P_Variables <- paste0("scas_parent_", 1:38)

FOR <- FOR %>%
  mutate(
    across(
      any_of(c(Scas_C_Variables, Scas_P_Variables)),
      ~ readr::parse_number(as.character(.x))
    )
  )

remove_indices <- c(11, 17, 26, 31, 38, 43)
Scas_C_Variables <- Scas_C_Variables[-remove_indices]


FOR <- FOR %>%
  # Recode 9s to NA
  mutate(across(all_of(Scas_C_Variables), ~na_if(.x, 9))) %>%
  mutate(across(all_of(Scas_P_Variables), ~na_if(.x, 9))) %>%
  
  # Compute total scores
  mutate(
    Scas_C_Total = rowSums(across(all_of(Scas_C_Variables)), na.rm = TRUE),
    Scas_P_Total = rowSums(across(all_of(Scas_P_Variables)), na.rm = TRUE)
  ) 