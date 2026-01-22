#1) Identify MFQ item names
mfq_y_vars <- c(paste0("mfq_0", 1:9), paste0("mfq_", 10:13))
mfq_p_vars <- paste0("mfq_p", 1:13)

#2) Clean items: keep only 0/1/2, else NA
FOR <- FOR %>%
  mutate(
    across(any_of(c(mfq_y_vars, mfq_p_vars)),
           ~ {
             x <- readr::parse_number(as.character(.x))
             dplyr::if_else(x > 2.5, NA_real_, x)
           })
  )

#Gen mfqytot and mfqptot
FOR <- FOR %>%
  rowwise() %>%
  mutate(
    # require at least 3 non-missing items (like min(3))
    mfqytot = {
      vals <- c_across(all_of(mfq_y_vars))
      if (sum(!is.na(vals)) < 3) NA_real_ else mean(vals, na.rm = TRUE) * 13
    },
    mfqptot = {
      vals <- c_across(all_of(mfq_p_vars))
      if (sum(!is.na(vals)) < 3) NA_real_ else mean(vals, na.rm = TRUE) * 13
    }
  ) %>%
  ungroup()
