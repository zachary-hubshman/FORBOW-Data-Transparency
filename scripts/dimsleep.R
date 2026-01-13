egen dimsleep = rowmean(zpsqitotal zcshtot zssrtot zsshtot)

*** correlation between sleep measures
pwcorr dimsleep zpsqitotal zcshtot zssrtot zpsqidist zpsqilaten zpsqitotal zcshsleepbeh zcshwake zssrtot zsshtot psqidurat psqidistb psqilaten psqidaydys psqihse psqislpqual psqimeds cshbedresistance cshsleepbeh cshwake

*** generate antecedent sleep
capture drop antsleep
gen antsleep = 0
replace antsleep = 1 if ccSleep == 1
* replace antsleep = 1 if clSleep == 1
replace antsleep = 1 if zpsqitotal>0.99 & zpsqitotal<9.99
replace antsleep = 1 if zcshtot>0.99 & zcshtot<9.99
replace antsleep = 1 if zssrtot>0.99 & zssrtot<9.99
replace antsleep = 1 if zsshtot>0.99 & zsshtot<9.99




#dimsleep needs psqi, csh, ssr, and ssh
egen zcshtot = rowmean(zcshsleepbeh zcshwake)


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


#Sleep Quality

#Medicine 

egen psqitotal = rowtotal(psqidurat psqidistb psqilaten psqidaydys psqihse psqislpqual psqimeds), missing



#SSR

#SSH