FOR <- FOR %>%
  mutate(subject_id = as.character(subject_id)) %>%
  filter(str_detect(subject_id, "^(\\d{10}|\\d{7}[MF][1-3])$"))
invalid_si <- c("9999999888", "9999999998", "8009999123")
FOR <- FOR %>%
  filter(!subject_id %in% invalid_si)


#Sex
FOR <- FOR %>%
  mutate(sex = recode(sex, `1` = 0, `2` = 1, .default = NA_real_)) %>%
  group_by(subject_id) %>%
  fill(sex, .direction = "downup") %>%
  ungroup()



#Race
race_lookup <- FOR %>%
  filter(!is.na(race)) %>%
  group_by(subject_id) %>%
  summarise(
    race_first = first(race),
    .groups = "drop"
  )

FOR <- FOR %>%
  left_join(race_lookup, by = "subject_id") %>%
  mutate(
    race = coalesce(race_first, race)
  ) %>%
  select(-race_first)
remove(race_lookup)



#Age
FOR <- FOR %>%
  mutate(dob = na_if(as.character(dob), ""),
         dob = ymd(dob, quiet = TRUE)) %>%
  group_by(subject_id) %>%
  arrange(assessment_date, .by_group = TRUE) %>%
  fill(dob, .direction = "downup") %>%   # fills both forward + backward
  ungroup()

FOR$assessment_date <- as.Date(FOR$assessment_date)
FOR$dob <- as.Date(FOR$dob)
FOR$age_days <- FOR$assessment_date - FOR$dob
FOR$age <- as.numeric(FOR$age_days) / 365.25


#SES
FOR$ohomerooms <- as.numeric(FOR$ohomerooms)
FOR$ohousepers <- as.numeric(FOR$ohousepers)

FOR <- FOR %>%
  mutate(
    # initialize SES domain flags
    sesmedul   = as.integer(medul_parent   > 2.5 & medul_parent   < 9),
    sesfedul   = as.integer(fedul_parent   > 2.5 & fedul_parent   < 9),
    sesown     = as.integer(oresown == 1),
    sesincome  = as.integer(oincome > 3.5 & oincome < 9),
    
    # correct the 43 coding error in household rooms
    ohomerooms = ifelse(ohomerooms == 43, 3, ohomerooms),
    
    # bedrooms-to-person ratio
    bpratio    = ohomerooms / ohousepers,
    sesbpratio = as.integer(bpratio > 0.9 & bpratio < 9),
    
    # total SES
    ses = sesmedul + sesfedul + sesown + sesincome + sesbpratio
  )

#PDS
##Male
#voice
FOR$gcqma_a2 <- as.numeric(FOR$gcqma_a2)
FOR$gcqma_a2 <- as.numeric(FOR$gcqma_a2)
#face growth
FOR$gcqma_a5_new <- as.numeric(FOR$gcqma_a5_new)
FOR$gcqmp_a5_new <- as.numeric(FOR$gcqmp_a5_new)

#skin changes
FOR$gcqma_a3_new <- as.numeric(FOR$gcqma_a3_new)
FOR$gcqmp_a3_new <- as.numeric(FOR$gcqmp_a3_new)

#body hair growth
FOR$gcqma_a4_new <- as.numeric(FOR$gcqma_a4_new)
FOR$gcqmp_a4_new <- as.numeric(FOR$gcqmp_a4_new)

FOR <- FOR %>%
  mutate(
    across(
      c(gcqma_a2, gcqmp_a2),
      ~ if_else(as.numeric(.) >= 4, NA_real_, as.numeric(.))
    ),
    across(
      c(gcqma_a3_new, gcqmp_a3_new,
        gcqma_a4_new, gcqmp_a4_new,
        gcqma_a5_new, gcqmp_a5_new),
      ~ if_else(as.numeric(.) >= 5, NA_real_, as.numeric(.))
    )
  )







##Female
###Menarche
FOR$gcqfa_d2 <- as.numeric(FOR$gcqfa_d2) 
FOR$gcqfa_d3 <- as.numeric(FOR$gcqfa_d3)
FOR$gcqfp_d2 <- as.numeric(FOR$gcqfp_d2) 
FOR$gcqfp_d3 <- as.numeric(FOR$gcqfp_d3)

FOR <- FOR %>%
  arrange(subject_id, time_point) %>%
  mutate(
    # Menarche age from adolescent or parent
    menarche_age = coalesce(gcqfa_a3, gcqfp_a3),
    # invalid if later than current age
    menarche_age = ifelse(menarche_age > age, NA, menarche_age)
  ) %>%
  group_by(subject_id) %>%
  mutate(
    # carry age forward
    menarche_age_fw = na.locf(menarche_age, na.rm = FALSE)
  ) %>%
  ungroup() %>%
  mutate(
    # direct yes/no
    menarche_yn = case_when(
      gcqfa_a2 == 1 | gcqfp_a2 == 1 ~ 1L,
      gcqfa_a2 == 0 | gcqfp_a2 == 0 ~ 0L,
      menarche_age_fw < age ~ 1L,
      TRUE ~ NA_integer_
    )
  ) %>%
  group_by(subject_id) %>%
  mutate(
    # once yes, always yes
    menarche_yn_fw = cummax(menarche_yn)
  ) %>%
  ungroup() %>%
  mutate(
    # sanity thresholds
    menarche_yn_fw = case_when(
      sex == 1 & age < 10 ~ 0L,
      sex == 1 & age > 15 ~ 1L,
      TRUE ~ menarche_yn_fw
    )
  )


FOR <- FOR %>%
  mutate(
    gcqfa_d2 = ifelse(gcqfa_d2 == 5, 0, gcqfa_d2),
    gcqfa_d3 = ifelse(gcqfa_d3 == 5, 0, gcqfa_d3),
    gcqfp_d2 = ifelse(gcqfp_d2 == 5, 0, gcqfp_d2),
    gcqfp_d3 = ifelse(gcqfp_d3 == 5, 0, gcqfp_d3)
  )

###Self-Report
FOR <- FOR %>%
  mutate(
    fa_sum = gcqfa_d2 + gcqfa_d3,
    puberty_pds_fa = case_when(
      fa_sum <= 2 & gcqfa_a2 == 0 ~ 1L,
      fa_sum == 3 & gcqfa_a2 == 0 ~ 2L,
      fa_sum > 3 & fa_sum < 7 & gcqfa_a2 == 0 ~ 3L,
      fa_sum <= 7 & gcqfa_a2 == 1 ~ 4L,
      fa_sum == 8 & gcqfa_a2 == 1 ~ 5L,
      TRUE ~ NA_integer_
    )
  )
###Parent Report
FOR <- FOR %>%
  mutate(
    fp_sum = gcqfp_d2 + gcqfp_d3,
    puberty_pds_fp = case_when(
      fp_sum <= 2 & gcqfp_a2 == 0 ~ 1L,
      fp_sum == 3 & gcqfp_a2 == 0 ~ 2L,
      fp_sum > 3 & fp_sum < 7 & gcqfp_a2 == 0 ~ 3L,
      fp_sum <= 7 & gcqfp_a2 == 1 ~ 4L,
      fp_sum == 8 & gcqfp_a2 == 1 ~ 5L,
      TRUE ~ NA_integer_
    )
  )

###Puberty stage
FOR <- FOR %>%
  mutate(
    puberty_pds_female = pmax(puberty_pds_fa, puberty_pds_fp, na.rm = TRUE)
  )

FOR <- FOR %>%
  mutate(
    puberty_pds = case_when(
      sex == "2" ~ puberty_pds_female,
      TRUE           ~ NA_integer_
    )
  )

###Handling missingness
FOR <- FOR %>%
  mutate(
    puberty_pds = case_when(
      menarche_yn == 1 & is.na(puberty_pds) ~ 5L,
      menarche_yn == 0 & gcqfp_d2 == 1 & is.na(puberty_pds) ~ 1L,
      menarche_yn == 0 & gcqfa_d2 == 3 & is.na(puberty_pds) ~ 3L,
      TRUE ~ puberty_pds
    )
  )

###Stage forward in time
FOR <- FOR %>%
  group_by(subject_id) %>%
  mutate(puberty_fw = cummax(puberty_pds)) %>%
  ungroup()



#Biological family?
FOR <- FOR %>%
  mutate(
    home_bio = if_else(
      home_biomoth == 2 | home_biofath == 2,
      1L,
      0L,
      missing = 0L
    )
  )






