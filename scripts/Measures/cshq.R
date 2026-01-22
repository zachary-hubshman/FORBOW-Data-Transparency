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