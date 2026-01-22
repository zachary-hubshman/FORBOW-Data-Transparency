#Family Risk
FOR$fid <- substr(FOR$subject_id, 5, 7)
FOR$iid <- substr(FOR$subject_id, 8, 11)
FOR_P <- FOR %>%
  filter(str_detect(subject_id, "F[1-3]|f[1-3]|M[1-3]|m[1-3]|\\$R")) %>%
  mutate(
    mdx = as.numeric(mdx),
    fdx = as.numeric(fdx),
    
    g_m = case_when(
      mdx == 4 ~ 1,
      mdx %in% c(2, 3) ~ 2,
      mdx == 1 ~ 3,
      mdx == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    g_f = case_when(
      fdx == 4 ~ 1,
      fdx %in% c(2, 3) ~ 2,
      fdx == 1 ~ 3,
      fdx == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    

    # 0 is lowest and 3 is highest, so take max.
    group = pmax(g_m, g_f, na.rm = TRUE),
    group = ifelse(is.infinite(group), NA_real_, group)  # pmax returns -Inf if both NA
  ) %>%
  group_by(subject_id) %>%
  mutate(
    # choose the group from a “parent row” if that exists, otherwise any non-NA group
    group = {
      gp <- group[is.na(time_point) & !is.na(group)]
      if (length(gp) > 0) gp[1] else {
        ga <- group[!is.na(group)]
        if (length(ga) > 0) ga[1] else NA_real_
      }
    }
  ) %>%
  ungroup()


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
