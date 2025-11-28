#Family Risk
FOR$fid <- substr(FOR$subject_id, 5, 7)
FOR$iid <- substr(FOR$subject_id, 8, 11)
FOR_P <- FOR %>%
  filter(str_detect(subject_id, "F[1-3]|f[1-3]|M[1-3]|m[1-3]|\\$R"))


FOR_P <- FOR_P %>%
  mutate(group = case_when(
    mdx == 4 ~ 1,
    mdx %in% c(2, 3) ~ 2,
    mdx == 1 ~ 3,
    mdx == 0 ~ 0,
    fdx == 4 ~ 1, 
    fdx %in% c(2,3) ~ 2,
    fdx == 1 ~ 3,
    fdx == 0 ~ 0,
    TRUE ~ NA_real_
  ))

FOR_P <- FOR_P %>%
  group_by(subject_id) %>%
  mutate(
    group = {
      g <- group[is.na(time_point)]
      if (length(g) == 0 || is.na(g[1])) 0 else g[1]
    }
  ) %>%
  ungroup()


table(FOR_P$medul)

fid_group_list <- FOR_P %>%
  group_by(fid) %>%
  summarise(
    group = if (all(is.na(group))) NA_real_ else max(group, na.rm = TRUE),
    medul_parent = if (all(is.na(medul))) NA_real_ else max(medul, na.rm = TRUE),
    fedul_parent = if (all(is.na(fedul))) NA_real_ else max(fedul, na.rm = TRUE),
    .groups = "drop"
  )

#Join back into full data set
FOR <- FOR %>%
  left_join(fid_group_list, by = "fid")

FOR <- FOR %>%
  mutate(fhr = case_when(
    group %in% c(1, 2, 3, 4, 5) ~ 1L,
    TRUE ~ 0L
  ))


rm(FOR_P, fid_group_list)
