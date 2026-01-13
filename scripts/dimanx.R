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
  ) %>%
  
  # Standardize total scores
  mutate(
    Scared_C_Total_z = as.numeric(scale(Scared_C_Total)),
    Scared_P_Total_z = as.numeric(scale(Scared_P_Total))
  )

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
  ) %>%
  
  # Standardize total scores
  mutate(
    Scas_C_z = as.numeric(scale(Scas_C_Total)),
    Scas_P_z = as.numeric(scale(Scas_P_Total))
  )

FOR <- FOR %>%
  # SCAS dimanx
  mutate(dimanx = rowMeans(across(c(Scas_C_z, Scas_P_z, Scared_C_Total_z, Scared_P_Total_z)), na.rm = TRUE)) 