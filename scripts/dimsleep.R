# egen dimsleep = rowmean(zpsqitotal zcshtot zssrtot zsshtot)
# 
# *** correlation between sleep measures
# pwcorr dimsleep zpsqitotal zcshtot zssrtot zpsqidist zpsqilaten zpsqitotal zcshsleepbeh zcshwake zssrtot zsshtot psqidurat psqidistb psqilaten psqidaydys psqihse psqislpqual psqimeds cshbedresistance cshsleepbeh cshwake
# 



#dimsleep needs psqi, csh, ssr, and ssh



library(dplyr)


#CSH
CSHQ_variables <- c(
  "csh1consistent","csh3alone","csh4otherbed","csh7parhelp","csh10refuses","csh12fear",
  "csh2fallasleep","csh13lack","csh16consamnt","csh15median","csh11dark","csh27away",
  "csh21moves","csh31wakeone","csh32waketwo","csh17wets","csh18talks","csh19restless",
  "csh20walks","csh23teeth","csh29awakens","csh30baddream","csh24snores","csh25breathing",
  "csh26gasps","csh34wakesup","csh36badmood","csh37otherwake","csh38longwake",
  "csh39alert","csh44tired","cshwatchtv","cshincar"
)

#Reverse-coded items 
reverse_vars <- c(
  "csh1consistent","csh2fallasleep","csh3alone",
  "csh15median","csh16consamnt","csh34wakesup"
)

#Variables needed for Z score for zCSH
sleepbeh_vars <- c("csh13lack","csh14excess","csh15median","csh16consamnt",
                   "csh19restless","csh21moves","csh28complains")
wake_vars <- c("csh31wakeone","csh32waketwo","csh40early")


#
FOR <- FOR %>%
  mutate(
    across(all_of(CSHQ_variables),
           ~ ifelse(.x %in% c(1, 2, 3), .x, NA_real_))
  ) %>%
  mutate(
    across(any_of(unique(c(CSHQ_variables, reverse_vars, sleepbeh_vars, wake_vars))),
           ~ suppressWarnings(as.numeric(as.character(.x)))),
    across(any_of(unique(c(CSHQ_variables, reverse_vars, sleepbeh_vars, wake_vars))),
           ~ na_if(na_if(.x, 98), 99)),
    across(all_of(reverse_vars),
           ~ dplyr::case_when(
             .x == 1 ~ 3,
             .x == 2 ~ 2,
             .x == 3 ~ 1,
             TRUE    ~ .x
           ))
  ) %>%
  mutate(
    CSHQTotal   = rowSums(pick(any_of(CSHQ_variables))),
    cshsleepbeh = rowSums(pick(any_of(sleepbeh_vars))),
    cshwake     = rowSums(pick(any_of(wake_vars))),
    zcshsleepbeh = as.numeric(scale(cshsleepbeh)),
    zcshwake     = as.numeric(scale(cshwake)),
    zcsh = rowMeans(
      cbind(zcshwake, zcshsleepbeh),
      na.rm = TRUE
    )
  )
##PSQI
#duration
FOR$psqi_4 <- as.numeric(FOR$psqi_4)
FOR <- FOR %>%
  mutate(
    psqidurat = case_when(
      psqi_4 > 23.99                 ~ NA_real_,
      psqi_4 > 6.99 & psqi_4 < 23.99 ~ 0,
      psqi_4 < 5                    ~ 3,
      psqi_4 < 6                    ~ 2,
      psqi_4 < 7                    ~ 1,
      TRUE                          ~ NA_real_
    )
  )

#Sleep disturbances
dist_vars <- c("psqi_5b","psqi_5c","psqi_5d","psqi_5e",
               "psqi_5f","psqi_5g","psqi_5h","psqi_5i","psqi_5j")

FOR <- FOR %>%
  mutate(
    across(all_of(dist_vars),
           ~ suppressWarnings(as.numeric(as.character(.x))))
  ) %>%
  mutate(
    psqdistscore = rowSums(pick(any_of(dist_vars)), na.rm = TRUE),
    psqidistb = case_when(
      psqdistscore == 0  ~ 0,
      psqdistscore < 9  ~ 1,
      psqdistscore < 18 ~ 2,
      psqdistscore < 99 ~ 3,
      TRUE              ~ NA_real_
    )
  )

#Latency
FOR <- FOR %>%
  mutate(
    across(all_of(c("psqi_2", "psqi_5a")),
           ~ suppressWarnings(as.numeric(as.character(.x))))
  ) %>%
  mutate(
    q2new = case_when(
      psqi_2 > 9999            ~ NA_real_,
      psqi_2 < 15.01           ~ 0,
      psqi_2 < 30.01           ~ 1,
      psqi_2 < 60              ~ 2,
      psqi_2 < 9999            ~ 3,
      TRUE                     ~ NA_real_
    ),
    slatv = rowSums(cbind(psqi_5a, q2new), na.rm = TRUE),
    psqilaten = case_when(
      slatv < 1  ~ 0,
      slatv < 3  ~ 1,
      slatv < 5  ~ 2,
      slatv < 7  ~ 3,
      TRUE       ~ NA_real_
    )
  )

#Daytime Sleepiness

FOR <- FOR %>%
  mutate(
    across(all_of(c("psqi_8", "psqi_9")),
           ~ suppressWarnings(as.numeric(as.character(.x))))
  ) %>%
  mutate(
    sdayv = ifelse(is.na(psqi_8) & is.na(psqi_9), NA_real_,
                   rowSums(cbind(psqi_8, psqi_9), na.rm = TRUE)),
    psqidaydys = case_when(
      sdayv < 1 ~ 0,
      sdayv < 3 ~ 1,
      sdayv < 5 ~ 2,
      sdayv < 7 ~ 3,
      TRUE      ~ NA_real_
    )
  )

#Sleep Efficiency
FOR <- FOR %>%
  mutate(
    psqi_1 = ifelse(str_detect(psqi_1, ":"), str_replace(psqi_1, ":", ""), psqi_1),
    psqi_3 = ifelse(str_detect(psqi_3, ":"), str_replace(psqi_3, ":", ""), psqi_3)
  ) %>%
  mutate(
    across(all_of(c("psqi_1","psqi_3","psqi_4")),
           ~ suppressWarnings(as.numeric(as.character(.x))))
  )

#A
FOR <- FOR %>%
  mutate(
    q1_min = ifelse(is.na(psqi_1), NA_real_,
                    (psqi_1 %/% 100) * 60 + (psqi_1 %% 100)),
    q3_min = ifelse(is.na(psqi_3), NA_real_,
                    (psqi_3 %/% 100) * 60 + (psqi_3 %% 100)),
    
    # signed difference (NOT abs)
    Diffmin_signed = ifelse(is.na(q1_min) | is.na(q3_min),
                            NA_real_,
                            q3_min - q1_min),
    
    # if negative, add 24h in minutes
    Diffmin = ifelse(is.na(Diffmin_signed),
                     NA_real_,
                     ifelse(Diffmin_signed < 0, Diffmin_signed + 1440, Diffmin_signed)),
    
    Diffhour = Diffmin / 60,
    newtib   = Diffhour,
    
    # assuming psqi_4 is HOURS slept
    tmphse = ifelse(is.na(psqi_4) | is.na(newtib) | newtib == 0,
                    NA_real_,
                    (psqi_4 / newtib) * 100)
  )
FOR <- FOR %>%
  mutate(
    psqihse = case_when(
      is.na(tmphse)          ~ NA_real_,
      tmphse > 85            ~ 0,
      tmphse <= 85 & tmphse > 75 ~ 1,
      tmphse <= 75 & tmphse > 65 ~ 2,
      tmphse <= 65           ~ 3
    )
  )



#Sleep Quality and Medicine
FOR <- FOR %>%
  mutate(
    psqislpqual = case_when(
      is.na(psqi_6) ~ NA_real_,
      TRUE ~ as.numeric(psqi_6)
    ),
    psqimeds = case_when(
      is.na(psqi_7) ~ NA_real_,
      TRUE ~ as.numeric(psqi_7)
    )
  )


FOR$psqitotal <- rowSums(
  FOR[, c("psqidurat",
          "psqidistb",
          "psqilaten",
          "psqidaydys",
          "psqihse",
          "psqislpqual",
          "psqimeds")]
)

FOR$zpsqitotal <- scale(FOR$psqitotal)

#SSR



##SSH
#Some SSH need to be subctracted by one
sub_SSH <- c("ssh15wake", "ssh19rate", "ssh20goodbad","ssh21reg",
             "ssh24blate","ssh24cmorn","ssh24daft","ssh24ewoke",
             "ssh24flate","ssh24gallnight","ssh24hnoon",
             "ssh24itired","ssh24jalarm","ssh24kdiffic","ssh24lbaddre",
             "ssh24mtobed","ssh24ndanger","ssh25bfall") 

#Reverse coded SSH
reverse_SSH <- c("ssh24ogoodsle", "ssh24asatis")


FOR <- FOR %>%
  mutate(
    across(all_of(c(sub_SSH, reverse_SSH)),
           ~ as.numeric(as.character(.x))))

FOR <- FOR %>%
  mutate(
    # subtract-1 items
    across(all_of(sub_SSH),
           ~ case_when(
             .x == 99           ~ NA_real_,
             .x %in% 1:5       ~ .x - 1,
             TRUE              ~ NA_real_
           )
    ),
    
    # reverse-coded items: 1..5 -> 4..0
    across(all_of(reverse_SSH),
           ~ case_when(
             .x == 99          ~ NA_real_,
             .x %in% 1:5      ~ 5 - .x,
             TRUE             ~ NA_real_
           )
    )
  )

ssh24_sleepwakeproblems <- c("ssh24asatis","ssh24blate","ssh24cmorn",
                 "ssh24daft","ssh24ewoke","ssh24flate",
                 "ssh24gallnight","ssh24hnoon","ssh24itired",
                 "ssh24jalarm","ssh24kdiffic","ssh24lbaddre",
                 "ssh24mtobed","ssh24ndanger","ssh24ogoodsle")

total_items <- c("ssh15wake","ssh19rate","ssh20goodbad",
                 "ssh21reg", ssh24_sleepwakeproblems, "ssh25bfall")
FOR <- FOR %>%
  rowwise() %>%
  mutate(
    sshtotal = sum(c_across(all_of(total_items))),
    sshtot   = sshtotal * 20
  ) %>% 
  ungroup()

FOR$zsshtot <- scale(FOR$sshtot)

table(FOR$zsshtot)
