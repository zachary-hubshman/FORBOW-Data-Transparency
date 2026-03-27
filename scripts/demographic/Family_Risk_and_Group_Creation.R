#Parent Education
FOR$fid <- substr(FOR$subject_id, 1, 7)
FOR$iid <- substr(FOR$subject_id, 8, 11)
FOR <- FOR %>%
  mutate(
    subject_id_mother = na_if(subject_id_mother, ""),
    subject_id_father = na_if(subject_id_father, "")
  )
FOR <- FOR %>% group_by(subject_id) %>%
  fill(subject_id_mother, .direction = "downup") %>%
  fill(subject_id_father, .direction = "downup") %>%
  ungroup()


FOR_P <- FOR %>%
  group_by(fid) %>%
  filter(any(str_detect(subject_id, "F[1-3]|f[1-3]|M[1-3]|m[1-3]"))) %>%
  ungroup()


fid_group_list <- FOR_P %>%
  group_by(subject_id) %>%
  summarise(
    medul_parent = if (all(is.na(medul))) NA_real_ else max(medul, na.rm = TRUE),
    fedul_parent = if (all(is.na(fedul))) NA_real_ else max(fedul, na.rm = TRUE),
    .groups = "drop"
  )

fid_list1 <- fid_group_list %>% filter((str_detect(subject_id, "F[1-3]|f[1-3]")))
fid_list2 <- fid_group_list %>% filter((str_detect(subject_id, "M[1-3]|m[1-3]")))
fid_list1 <- fid_list1 %>% rename(subject_id_father = subject_id)  %>% select(subject_id_father, fedul_parent)
fid_list2 <- fid_list2 %>% rename(subject_id_mother = subject_id)  %>% select(subject_id_mother, medul_parent)


#Join back into full data set
FOR <- FOR %>%
  left_join(fid_list1, by = "subject_id_father")
FOR <- FOR %>%
  left_join(fid_list2, by = "subject_id_mother")

rm(FOR_P, fid_group_list)

#Group with vars
vars$subject_id <- vars$Subject_ID
vars <- vars %>% select(subject_id, group)
vars$subject_id <- as.character(vars$subject_id)
vars <- vars %>% distinct(subject_id, group)
FOR <- FOR %>%
  left_join(vars, by = "subject_id")
rm(vars) 