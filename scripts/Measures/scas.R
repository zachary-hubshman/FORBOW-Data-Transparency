# helper: row sum that returns NA if ALL items are missing
row_sum_na <- function(df) {
  n_present <- rowSums(!is.na(df))
  s <- rowSums(df, na.rm = TRUE)
  ifelse(n_present == 0, NA_real_, s)
}

# helper: row mean that returns NA if ALL items are missing
row_mean_na <- function(df) {
  n_present <- rowSums(!is.na(df))
  m <- rowMeans(df, na.rm = TRUE)
  ifelse(n_present == 0, NA_real_, m)
}

####
####Scas
####

Scas_C_Variables <- paste0("scas_child_", 1:44)
Scas_P_Variables <- paste0("scas_parent_", 1:38)

remove_indices <- c(11, 17, 26, 31, 38, 43)
Scas_C_Variables <- Scas_C_Variables[-remove_indices]

FOR <- FOR %>%
  mutate(
    across(any_of(c(Scas_C_Variables, Scas_P_Variables)), ~ {
      x <- parse_number(as.character(.x))
      if_else(!is.na(x) & x > 4, NA_real_, as.numeric(x))
    }),
    Scas_C_Total = row_sum_na(pick(any_of(Scas_C_Variables))),
    Scas_P_Total = row_sum_na(pick(any_of(Scas_P_Variables)))
  )

FOR$Scared_C_Tot_Z <- scale(FOR$Scared_C_Total)
FOR$Scared_P_Tot_Z <- scale(FOR$Scared_P_Total)