
##Creating Maltreatment Variables
FOR <- FOR %>%
  mutate(
    ## Physical maltreatment
    maltp = as.integer(
      rowSums(across(c(jvq10_cv, jvq10_pv,
                       jvq11_cv, jvq11_pv),
                     ~ . == 2), na.rm = TRUE) > 0
    ),
    ## Emotional maltreatment
    malte = as.integer(
      rowSums(across(c(jvq12_cv, jvq12_pv),
                     ~ . == 2), na.rm = TRUE) > 0 |
        cecatorment11 == 1
    ),
    ## Neglect
    maltn = as.integer(
      rowSums(across(c(jvq13_cv, jvq13_pv,
                       jvq41_cv, jvq41_pv,
                       jvq42_cv, jvq42_pv,
                       jvq43_cv, jvq43_pv,
                       jvq44_cv, jvq44_pv,
                       jvq45_cv, jvq45_pv),
                     ~ . == 2), na.rm = TRUE) > 0 |
        cecabasneed11 == 1 |
        cecabasneed16 == 1
    ),
    ## Custody disruption
    custodydisp = as.integer(
      rowSums(across(c(jvq14_cv, jvq14_pv), ~ . == 2), na.rm = TRUE) > 0
    ),
    ## Sexual maltreatment
    malts = as.integer(
      rowSums(across(c(jvq25_cv, jvq25_pv,
                       jvq26_cv, jvq26_pv,
                       jvq28_cv, jvq28_pv),
                     ~ . == 2), na.rm = TRUE) > 0 |
        cecaunwsex11 == 1 |
        cecaunwsex16 == 1
    ),
    
    ## Violence exposure
    maltv = as.integer(
      rowSums(across(c(jvq32_cv, jvq32_pv,
                       jvq33_cv, jvq33_pv,
                       jvq34_cv, jvq34_pv,
                       jvq35_cv, jvq35_pv,
                       jvq36_cv, jvq36_pv,
                       jvq37_cv, jvq37_pv),
                     ~ . == 2), na.rm = TRUE) > 0 |
        cecafreqarg11 == 1 |
        cecafreqarg16 == 1
    )
  )



##Overall maltreatment at that visit
FOR <- FOR %>%
  mutate(
    malt = as.integer(maltp == 1 | malte == 1 | malts == 1 | maltn == 1),
    #Have malt include violence too
    bmalt = as.integer(malt == 1 | maltv == 1)
  )


##Create the lifetime maltreatment variable
FOR <- FOR %>%
  group_by(subject_id) %>%
  mutate(
    ## malt subtype lifetime
    maltp_ever = as.integer(any(maltp == 1, na.rm = TRUE)),
    malte_ever = as.integer(any(malte == 1, na.rm = TRUE)),
    maltn_ever = as.integer(any(maltn == 1, na.rm = TRUE)),
    malts_ever = as.integer(any(malts == 1, na.rm = TRUE)),
    maltv_ever = as.integer(any(maltv == 1, na.rm = TRUE)),
    custody_ever = as.integer(any(custodydisp == 1, na.rm = TRUE)),
    
    ## overall malt (physical, emotional, neglect, sexual)
    malt_ever = as.integer(
      any(maltp == 1 | malte == 1 | malts == 1 | maltn == 1, na.rm = TRUE)
    ),
    
    ## broadened malt including violence
    bmalt_ever = as.integer(
      any(maltp == 1 | malte == 1 | malts == 1 | maltn == 1 | maltv == 1, na.rm = TRUE)
    )
  ) %>%
  ungroup()


###Bully
#Generates bullying_ever. Which finds out if someone hsa either previously or currently is experiencing bullying.

FOR <- FOR %>%
  mutate(
    bullying = case_when(
      obully == 4 ~ 1L,
      obully == 3 ~ 1L,
      obully == 2 & obullserv %in% c(3, 4) ~ 1L,
      obully > 4.5 ~ NA_integer_,
      age < 4.99 ~ NA_integer_,
      TRUE ~ 0L
    )
  )

FOR <- FOR %>%
  mutate(
    cyber = case_when(
      ocyber1 > 1.5 & ocyber1 < 8 ~ 1L,
      ocyber2 > 1.5 & ocyber2 < 8 ~ 1L,
      ocyber3 > 1.5 & ocyber3 < 8 ~ 1L,
      TRUE ~ 0L
    )
  )

##This fidns if someone previously was bullied
FOR <- FOR %>%
  arrange(subject_id, time_point) %>%
  group_by(subject_id) %>%
  mutate(bullying_ever = cummax(replace(bullying, is.na(bullying), 0))) %>%
  ungroup()


###Adveristy
#Creates adversity measure. If an individual was bullied, has experiened maltreatment, or a custody dispute, then they get adversity as 1.
FOR <- FOR %>%
  mutate(
    adversity = case_when(
      malt > 9 | bullying > 9 ~ NA_integer_,
      bmalt == 1 | custodydisp == 1 | bullying == 1 ~ 1L,
      TRUE ~ 0L
    )
  )


###Polye
FOR <- FOR %>%
  mutate(
    rsesmedul    = ifelse(sesmedul == 1, 0L, ifelse(sesmedul == 0, 1L, NA_integer_)),
    rsesfedul    = ifelse(sesfedul == 1, 0L, ifelse(sesfedul == 0, 1L, NA_integer_)),
    rsesown      = ifelse(sesown == 1, 0L, ifelse(sesown == 0, 1L, NA_integer_)),
    rsesincome   = ifelse(sesincome == 1, 0L, ifelse(sesincome == 0, 1L, NA_integer_)),
    rsesbpratio  = ifelse(sesbpratio == 1, 0L, ifelse(sesbpratio == 0, 1L, NA_integer_))
  )

FOR <- FOR %>% 
  mutate(
    polye      = rowMeans(across(c(malts, maltp, malte, maltn, maltv, bullying, rsesmedul, rsesfedul, rsesown, rsesincome)), na.rm = TRUE),
    polyvicte  = rowMeans(across(c(malts, maltp, malte, maltn, maltv, bullying)), na.rm = TRUE),
    polysese   = rowMeans(across(c(rsesmedul, rsesfedul, rsesown, rsesincome)), na.rm = TRUE),
    polyv      = rowMeans(across(c(malts, maltp, malte, maltn, maltv, bullying, cyber)), na.rm = TRUE),
    c4polyv    = case_when(polyv > 3.5 & polyv < 9 ~ 1L, TRUE ~ 0L),
    zpolye     = (polye - mean(polye, na.rm = TRUE)) / sd(polye, na.rm = TRUE)
  )
