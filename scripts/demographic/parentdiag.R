#Parent Diagnosis and Family Risk
FOR$fid <- substr(FOR$subject_id, 5, 7)
FOR$iid <- substr(FOR$subject_id, 8, 11)
FOR_P <- FOR %>%
  filter(str_detect(subject_id, "F[1-3]|f[1-3]|M[1-3]|m[1-3]|\\$R"))

#Main Parent Diagnosis
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
    )) %>%
  mutate(fhr = case_when(
    group %in% c(1,2,3,4,5) ~ 1L,
    TRUE ~ 0L
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

##Specific Diagnosis
FOR_P <- FOR_P %>%
  mutate(
    mdd_p = case_when(
      mdx == 4 | fdx == 4 ~ 1L,  # severe parent dx → high-risk
      if_any(c(mcodxm_1:mcodxm_4, fcodxm_1:fcodxm_4), ~ . == 1) ~ 1L,
      TRUE ~ 0L
    )
  )


FOR_P <- FOR_P %>%
  mutate(
    bp_p = case_when(
      mdx %in% c(2,3) | fdx %in% c(2,3) ~ 1L,  
      if_any(c(mcodxm_1:mcodxm_4, fcodxm_1:fcodxm_4), ~ . %in% c(4,5)) ~ 1L,
      TRUE ~ 0L
    )
  )

FOR_P <- FOR_P %>%
  mutate(
    anx_p = case_when(
      if_any(c(mcodxm_1:mcodxm_4, fcodxm_1:fcodxm_4), ~ . %in% c(20)) ~ 1L,
      TRUE ~ 0L
    )
  )

FOR_P <- FOR_P %>%
  mutate(
    adhd_p = case_when(
      if_any(c(mcodxm_1:mcodxm_4, fcodxm_1:fcodxm_4), ~ . %in% c(31,32,33)) ~ 1L,
      TRUE ~ 0L
    )
  )

FOR_P <- FOR_P %>%
  mutate(
    behavior_p = case_when(
      if_any(c(mcodxm_1:mcodxm_4, fcodxm_1:fcodxm_4), ~ . %in% c(31,32,33,34, 35)) ~ 1L,
      TRUE ~ 0L
    )
  )

FOR_P <- FOR_P %>%
  mutate(
    behavior_p = case_when(
      if_any(c(mcodxm_1:mcodxm_4, fcodxm_1:fcodxm_4), ~ . %in% c(38,40,42)) ~ 1L,
      TRUE ~ 0L
    )
  )

FOR_P <- FOR_P %>%
  mutate(
    psy_p = case_when(
      mdx %in% c(1) | fdx %in% c(1) ~ 1L,  
      if_any(c(mcodxm_1:mcodxm_4, fcodxm_1:fcodxm_4), ~ . %in% c(4,5)) ~ 1L,
      TRUE ~ 0L
    )
  )

##Merge Back
fid_group_list <- FOR_P %>%
  group_by(fid) %>%
  summarise(
    group = if (all(is.na(group))) NA_real_ else max(group, na.rm = TRUE),
    fhr = if (all(is.na(fhr))) NA_real_ else max(fhr, na.rm = TRUE),
    medul_parent = if (all(is.na(medul))) NA_real_ else max(medul, na.rm = TRUE),
    fedul_parent = if (all(is.na(fedul))) NA_real_ else max(fedul, na.rm = TRUE),
    mdd_p = if (all(is.na(mdd_p))) NA_real_ else max(mdd_p, na.rm = TRUE),
    bp_p = if (all(is.na(bp_p))) NA_real_ else max(bp_p, na.rm = TRUE),
    psy_p = if (all(is.na(psy_p))) NA_real_ else max(psy_p, na.rm = TRUE),
    anx_p = if (all(is.na(anx_p))) NA_real_ else max(anx_p, na.rm = TRUE),
    adhd_p = if (all(is.na(adhd_p))) NA_real_ else max(adhd_p, na.rm = TRUE),
    behavior_p = if (all(is.na(behavior_p))) NA_real_ else max(behavior_p, na.rm = TRUE),
    .groups = "drop"
  )

#Join back into full data set
FOR <- FOR %>%
  left_join(fid_group_list, by = "fid")

FOR




# 1 "Major Depressive Disorder" ///
# 2 "Dysthymic Disorder" ///
# 3 "Mood Disorder NOS" ///
# 4 "Bipolar disorder I" ///
# 5 "Bipolar disorder II" ///
# 6 "Cyclothymic Disorder" ///
# 7 "Bipolar disorder NOS" ///
# 8 "Schizophrenia" ///
# 9 "Schizoaffective" ///
# 10 "Schizophreniform" ///
# 11 "Delusional Disorder" ///
# 12 "Brief Psychotic Disorder" ///
# 13 "Psychotic Disorder NOS" ///
# 14 "Psychotic disorder substance induced" ///
# 15 "Separation Anxiety" ///
# 16 "Specific Phobia" ///
# 17 "Social Phobia" ///
# 18 "Agoraphobia" ///
# 19 "Panic disorder" ///
# 20 "GAD" ///
# 21 "OCD" ///
# 22 "PTSD" ///
# 23 "Acute Stress" ///
# 24 "Anxiety Disorder NOS" ///
# 25 "Panic Attack" ///
# 26 "Enuresis" ///
# 27 "Encopresis" ///
# 28 "Anorexia Nervosa" ///
# 29 "Bulimia" ///
# 30 "Eating Disorder NOS" ///
# 31 "ADHD Combined" ///
# 32 "ADHD Inattentive" ///
# 33 "ADHD Hyperactive-Imp" ///
# 34 "ODD" ///
# 35 "Conduct Disorder" ///
# 36 "Tic Disorder Chronic" ///
# 37 "Tic Disorder Transient" ///
# 38 "Alcohol Abuse" ///
# 39 "Alcohol Dependence" ///
# 40 "Cannabis abuse" ///
# 41 "Cannabis dependence" ///
# 42 "Substance Abuse" ///
# 43 "Substance Dependence" ///
# 44 "Autistic Disorder" ///
# 45 "Aspergers Disorder" ///
# 46 "PDD NOS" ///
# 98 "Other Disorder" ///
# 99 "No response"



