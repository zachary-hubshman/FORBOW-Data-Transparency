ari_s <- paste0("ari_self_", 1:7)
ari_p <- paste0("ari_par_", 1:7)

FOR <- FOR %>%
  mutate(
    across(all_of(ari_s), ~ ifelse(.x > 3.5, NA_real_, as.numeric(.x)))
  )
FOR <- FOR %>%
  mutate(
    across(all_of(ari_p), ~ ifelse(.x > 3.5, NA_real_, as.numeric(.x)))
  )

#Calculate total ari_s and ari_p
FOR <- FOR %>%
  mutate(
    ari_s_tot = rowSums(across(all_of(ari_s)), na.rm = FALSE),
    ari_p_tot = rowSums(across(all_of(ari_p)), na.rm = FALSE)
  ) 