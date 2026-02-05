
#PDS
##Female
###Menarche
library(dplyr)

FOR <- FOR %>%
  # ------------------------------------------------------------
# 0) Standardize column names to lowercase (keeps underscores)
# ------------------------------------------------------------

# ------------------------------------------------------------
# 1) Helper recodes: treat "5" as 0 (per Stata recode (5=0))
#    and treat 9 as missing for SMS items (per comments)
# ------------------------------------------------------------
mutate(
  across(
    any_of(c(
      # Female PDS items
      "gcqfa_d2","gcqfa_d3","gcqfp_d2","gcqfp_d3",
      # Male PDS items
      "gcqma_a3_new","gcqma_a4_new","gcqma_a5_new",
      "gcqmp_a3_new","gcqmp_a4_new","gcqmp_a5_new"
    )),
    ~ dplyr::if_else(.x == 5, 0, as.numeric(.x))
  ),
  across(
    any_of(c(
      # Female SMS items
      "gcqfa_b","gcqfa_c","gcqfp_b","gcqfp_c",
      # Male SMS items
      "gcqma_b","gcqma_c","gcqmp_b","gcqmp_c"
    )),
    ~ dplyr::if_else(.x == 9, NA_real_, as.numeric(.x))
  )
) %>%
  
  # ------------------------------------------------------------
# 2) FEMALES: PDS (adolescent report = _fa, parent report = _fp)
#    Logic matches the Stata replace rules.
# ------------------------------------------------------------
mutate(
  pds_sum_fa = if_else(!is.na(gcqfa_d2) & !is.na(gcqfa_d3),
                       gcqfa_d2 + gcqfa_d3, NA_real_),
  pds_sum_fp = if_else(!is.na(gcqfp_d2) & !is.na(gcqfp_d3),
                       gcqfp_d2 + gcqfp_d3, NA_real_),
  
  puberty_scorepds_fa = case_when(
    pds_sum_fa <= 2 & gcqfa_a2 == 0 ~ 1,
    pds_sum_fa == 3 & gcqfa_a2 == 0 ~ 2,
    pds_sum_fa >  3 & pds_sum_fa < 7 & gcqfa_a2 == 0 ~ 3,
    pds_sum_fa <= 7 & gcqfa_a2 == 1 ~ 4,
    pds_sum_fa == 8 & gcqfa_a2 == 1 ~ 5,
    TRUE ~ NA_real_
  ),
  
  puberty_scorepds_fp = case_when(
    pds_sum_fp <= 2 & gcqfp_a2 == 0 ~ 1,
    pds_sum_fp == 3 & gcqfp_a2 == 0 ~ 2,
    pds_sum_fp >  3 & pds_sum_fp < 7 & gcqfp_a2 == 0 ~ 3,
    pds_sum_fp <= 7 & gcqfp_a2 == 1 ~ 4,
    pds_sum_fp == 8 & gcqfp_a2 == 1 ~ 5,
    TRUE ~ NA_real_
  ),
  
  # "total PDS score for females" = take whichever category appears in FA or FP
  puberty_scorepds = case_when(
    puberty_scorepds_fa == 1 | puberty_scorepds_fp == 1 ~ 1,
    puberty_scorepds_fa == 2 | puberty_scorepds_fp == 2 ~ 2,
    puberty_scorepds_fa == 3 | puberty_scorepds_fp == 3 ~ 3,
    puberty_scorepds_fa == 4 | puberty_scorepds_fp == 4 ~ 4,
    puberty_scorepds_fa == 5 | puberty_scorepds_fp == 5 ~ 5,
    TRUE ~ NA_real_
  ),
  
  menarcheyn = case_when(
    gcqfa_a2 == 1 ~ 1,   # adolescent overrides
    gcqfa_a2 == 0 ~ 0,
    gcqfp_a2 == 1 ~ 1,   # fallback to parent
    gcqfp_a2 == 0 ~ 0,
    TRUE          ~ NA_real_
  ),
  
  
  # ad-hoc fixes from Stata (applied when puberty_scorepds was missing there)
  puberty_scorepds = case_when(
    menarcheyn == 1 & is.na(puberty_scorepds) ~ 5,
    menarcheyn == 0 & gcqfp_d2 == 1 & is.na(puberty_scorepds) ~ 1,
    menarcheyn == 0 & gcqfa_d2 == 3 & is.na(puberty_scorepds) ~ 3,
    TRUE ~ puberty_scorepds
  )
) %>%
  
  # ------------------------------------------------------------
# 3) FEMALES: SMS
#    FA_sms = mean(gcqfa_b, gcqfa_c); FP_sms = mean(gcqfp_b, gcqfp_c)
#    Then bin to 1..5 using Stata cutpoints.
# ------------------------------------------------------------
mutate(
  fa_sms = rowMeans(across(any_of(c("gcqfa_b","gcqfa_c"))), na.rm = FALSE),
  fp_sms = rowMeans(across(any_of(c("gcqfp_b","gcqfp_c"))), na.rm = FALSE),
  
  puberty_scoresms_fa = case_when(
    fa_sms == 1 ~ 1,
    fa_sms > 1 & fa_sms < 2.5 ~ 2,
    fa_sms > 2 & fa_sms < 3.5 ~ 3,
    fa_sms > 3 & fa_sms < 4.5 ~ 4,
    fa_sms > 4 & fa_sms < 5.5 ~ 5,
    TRUE ~ NA_real_
  ),
  puberty_scoresms_fp = case_when(
    fp_sms == 1 ~ 1,
    fp_sms > 1 & fp_sms < 2.5 ~ 2,
    fp_sms > 2 & fp_sms < 3.5 ~ 3,
    fp_sms > 3 & fp_sms < 4.5 ~ 4,
    fp_sms > 4 & fp_sms < 5.5 ~ 5,
    TRUE ~ NA_real_
  ),
  
  puberty_scoresms = case_when(
    puberty_scoresms_fa == 1 | puberty_scoresms_fp == 1 ~ 1,
    puberty_scoresms_fa == 2 | puberty_scoresms_fp == 2 ~ 2,
    puberty_scoresms_fa == 3 | puberty_scoresms_fp == 3 ~ 3,
    puberty_scoresms_fa == 4 | puberty_scoresms_fp == 4 ~ 4,
    puberty_scoresms_fa == 5 | puberty_scoresms_fp == 5 ~ 5,
    TRUE ~ NA_real_
  ),
  
  # Stata: if menarche yes and missing -> post; if no menarche and missing -> pre
  puberty_scoresms = case_when(
    menarcheyn == 1 & is.na(puberty_scoresms) ~ 5,
    menarcheyn == 0 & is.na(puberty_scoresms) ~ 1,
    TRUE ~ puberty_scoresms
  )
) %>%
  
  # ------------------------------------------------------------
# 4) FEMALES: composite puberty_score = bin(sum of sms+pds)
#    then fall back to whichever exists if still missing.
# ------------------------------------------------------------
mutate(
  puberty_score = case_when(
    (puberty_scoresms + puberty_scorepds) %in% c(1, 2) ~ 1,
    (puberty_scoresms + puberty_scorepds) %in% c(3, 4) ~ 2,
    (puberty_scoresms + puberty_scorepds) %in% c(5, 6) ~ 3,
    (puberty_scoresms + puberty_scorepds) %in% c(7, 8) ~ 4,
    (puberty_scoresms + puberty_scorepds) %in% c(9, 10) ~ 5,
    TRUE ~ NA_real_
  ),
  puberty_score = if_else(is.na(puberty_score) & !is.na(puberty_scoresms), puberty_scoresms, puberty_score),
  puberty_score = if_else(is.na(puberty_score) & !is.na(puberty_scorepds), puberty_scorepds, puberty_score)
) %>%
  
  # ------------------------------------------------------------
# 5) MALES: PDS from sums of (a3_new + a4_new + a5_new)
#    Includes the same "exclude a 3" / "exclude a 4" logic as Stata.
# ------------------------------------------------------------
mutate(
  pds_sum_ma = if_else(!is.na(gcqma_a3_new) & !is.na(gcqma_a4_new) & !is.na(gcqma_a5_new),
                       gcqma_a3_new + gcqma_a4_new + gcqma_a5_new, NA_real_),
  pds_sum_mp = if_else(!is.na(gcqmp_a3_new) & !is.na(gcqmp_a4_new) & !is.na(gcqmp_a5_new),
                       gcqmp_a3_new + gcqmp_a4_new + gcqmp_a5_new, NA_real_),
  
  puberty_scorepds_ma = case_when(
    pds_sum_ma == 3 ~ 1,
    (pds_sum_ma %in% c(4,5)) & !(gcqma_a3_new == 3 | gcqma_a4_new == 3 | gcqma_a5_new == 3) ~ 2,
    (pds_sum_ma %in% c(6,7,8)) & !(gcqma_a3_new == 4 | gcqma_a4_new == 4 | gcqma_a5_new == 4) ~ 3,
    pds_sum_ma > 8 & pds_sum_ma < 12 ~ 4,
    pds_sum_ma == 12 ~ 5,
    TRUE ~ NA_real_
  ),
  
  puberty_scorepds_mp = case_when(
    pds_sum_mp == 3 ~ 1,
    (pds_sum_mp %in% c(4,5)) & !(gcqmp_a3_new == 3 | gcqmp_a4_new == 3 | gcqmp_a5_new == 3) ~ 2,
    (pds_sum_mp %in% c(6,7,8)) & !(gcqmp_a3_new == 4 | gcqmp_a4_new == 4 | gcqmp_a5_new == 4) ~ 3,
    pds_sum_mp > 8 & pds_sum_mp < 12 ~ 4,
    pds_sum_mp == 12 ~ 5,
    TRUE ~ NA_real_
  ),
  
  puberty_scorepds_male = case_when(
    puberty_scorepds_ma == 1 | puberty_scorepds_mp == 1 ~ 1,
    puberty_scorepds_ma == 2 | puberty_scorepds_mp == 2 ~ 2,
    puberty_scorepds_ma == 3 | puberty_scorepds_mp == 3 ~ 3,
    puberty_scorepds_ma == 4 | puberty_scorepds_mp == 4 ~ 4,
    puberty_scorepds_ma == 5 | puberty_scorepds_mp == 5 ~ 5,
    TRUE ~ NA_real_
  )
) %>%
  
  # ------------------------------------------------------------
# 6) MALES: SMS + composite puberty_score_male (same binning rule)
# ------------------------------------------------------------
mutate(
  ma_sms = rowMeans(across(any_of(c("gcqma_b","gcqma_c"))), na.rm = FALSE),
  mp_sms = rowMeans(across(any_of(c("gcqmp_b","gcqmp_c"))), na.rm = FALSE),
  
  puberty_scoresms_ma = case_when(
    ma_sms == 1 ~ 1,
    ma_sms > 1 & ma_sms < 2.5 ~ 2,
    ma_sms > 2 & ma_sms < 3.5 ~ 3,
    ma_sms > 3 & ma_sms < 4.5 ~ 4,
    ma_sms > 4 & ma_sms < 5.5 ~ 5,
    TRUE ~ NA_real_
  ),
  puberty_scoresms_mp = case_when(
    mp_sms == 1 ~ 1,
    mp_sms > 1 & mp_sms < 2.5 ~ 2,
    mp_sms > 2 & mp_sms < 3.5 ~ 3,
    mp_sms > 3 & mp_sms < 4.5 ~ 4,
    mp_sms > 4 & mp_sms < 5.5 ~ 5,
    TRUE ~ NA_real_
  ),
  
  puberty_scoresms_male = case_when(
    puberty_scoresms_ma == 1 | puberty_scoresms_mp == 1 ~ 1,
    puberty_scoresms_ma == 2 | puberty_scoresms_mp == 2 ~ 2,
    puberty_scoresms_ma == 3 | puberty_scoresms_mp == 3 ~ 3,
    puberty_scoresms_ma == 4 | puberty_scoresms_mp == 4 ~ 4,
    puberty_scoresms_ma == 5 | puberty_scoresms_mp == 5 ~ 5,
    TRUE ~ NA_real_
  ),
  
  puberty_score_male = case_when(
    (puberty_scoresms_male + puberty_scorepds_male) %in% c(1, 2) ~ 1,
    (puberty_scoresms_male + puberty_scorepds_male) %in% c(3, 4) ~ 2,
    (puberty_scoresms_male + puberty_scorepds_male) %in% c(5, 6) ~ 3,
    (puberty_scoresms_male + puberty_scorepds_male) %in% c(7, 8) ~ 4,
    (puberty_scoresms_male + puberty_scorepds_male) %in% c(9, 10) ~ 5,
    TRUE ~ NA_real_
  ),
  puberty_score_male = if_else(is.na(puberty_score_male) & !is.na(puberty_scorepds_male), puberty_scorepds_male, puberty_score_male),
  puberty_score_male = if_else(is.na(puberty_score_male) & !is.na(puberty_scoresms_male), puberty_scoresms_male, puberty_score_male)
) %>%
  
  # ------------------------------------------------------------
# 7) Puberty onset (0/1) + unified 1..5 puberty score across sexes
# ------------------------------------------------------------
mutate(
  puberty_onset = case_when(
    puberty_score %in% c(1,2) | puberty_score_male %in% c(1,2) ~ 0,
    puberty_score %in% c(3,4,5) | puberty_score_male %in% c(3,4,5) ~ 1,
    TRUE ~ NA_real_
  ),
  puberty_onset = case_when(
    is.na(puberty_onset) & last == 1 & age < 11 ~ 0,
    is.na(puberty_onset) & last == 1 & age > 17 ~ 1,
    TRUE ~ puberty_onset
  ),
  
  puberty = puberty_score,
  puberty = if_else(is.na(puberty) & !is.na(puberty_score_male), puberty_score_male, puberty),
  puberty = if_else(is.na(puberty) & age < 11, 1, puberty),
  puberty = if_else(is.na(puberty) & age > 17, 5, puberty)
)

###Stage forward in time
FOR <- FOR %>%
  group_by(subject_id) %>%
  mutate(puberty_fw = cummax(puberty_pds)) %>%
  ungroup()