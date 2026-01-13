#Sex
FOR <- FOR %>%
  group_by(subject_id) %>%
  mutate(dob = dob[which(is.na(time_point))][1]) %>%
  ungroup()
FOR <- FOR %>%
  group_by(subject_id) %>%
  mutate(sex = sex[which(is.na(time_point))][1]) %>%
  ungroup()
FOR <- FOR %>%
  mutate(sex = case_when(
    sex == 1 ~ 0,
    sex == 2 ~ 1
  ))
FOR <- FOR %>%
  group_by(subject_id) %>%
  filter(all(!is.na(dob) & dob != "")) %>%  # Keep only groups where *all* dob are not missing or empty
  ungroup()

invalid_si <- c("9999999888", "9999999998")
FOR <- FOR %>%
  filter(!subject_id %in% invalid_si)

#Age
FOR$assessment_date <- as.Date(FOR$assessment_date)
FOR$dob <- as.Date(FOR$dob)
FOR$age_days <- FOR$assessment_date - FOR$dob
FOR$age <- as.numeric(FOR$age_days) / 365.25


#SES
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
##Female
###Menarche
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
    puberty_pds = pmax(puberty_pds_fa, puberty_pds_fp, na.rm = TRUE)
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





