library(dplyr)
library(readr)
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
####Scared
####
Scared_C_Variables <- paste0("scared_child_", 1:41)
Scared_P_Variables <- paste0("scared_parent_", 1:41)

FOR <- FOR %>%
  mutate(
    across(any_of(c(Scared_C_Variables, Scared_P_Variables)), ~ {
      x <- parse_number(as.character(.x))
      if_else(!is.na(x) & x > 4, NA_real_, as.numeric(x))
    }),
    Scared_C_Total = row_sum_na(pick(any_of(Scared_C_Variables))),
    Scared_P_Total = row_sum_na(pick(any_of(Scared_P_Variables)))
  )