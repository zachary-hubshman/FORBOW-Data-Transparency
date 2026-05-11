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
####MFQ
####
mfq_y_vars <- c(paste0("mfq_0", 1:9), paste0("mfq_", 10:13))
mfq_p_vars <- paste0("mfq_p", 1:13)

FOR <- FOR %>%
  mutate(
    across(any_of(c(mfq_y_vars, mfq_p_vars)), ~ {
      x <- parse_number(as.character(.x))
      if_else(!is.na(x) & x > 4, NA_real_, as.numeric(x))
    }),
    mfq_y_tot = row_sum_na(pick(any_of(mfq_y_vars))),
    mfq_p_tot = row_sum_na(pick(any_of(mfq_p_vars)))
  )

FOR$mfq_y_tot_z <- scale(FOR$mfq_y_tot)
FOR$mfq_p_tot_z <- scale(FOR$mfq_p_tot)

FOR <- FOR %>%
  mutate(
    dimdep        = row_mean_na(pick(any_of(c("mfq_y_tot_z", "mfq_p_tot_z"))))
  )
