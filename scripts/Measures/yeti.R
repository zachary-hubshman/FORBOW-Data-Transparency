#yeti setup
yeti_vars <- paste0("yeti", 1:26)

anx_items    <- c("yeti1", "yeti5", "yeti7", "yeti10", "yeti13")
dep_items    <- c("yeti2", "yeti4", "yeti9", "yeti12", "yeti14")
afflab_items <- c("yeti3", "yeti6", "yeti8", "yeti11", "yeti15", "yeti18")
ple_items    <- paste0("yeti", 19:22)
basic_items  <- paste0("yeti", 23:26)
sleep_items  <- c("yeti16", "yeti17")

FOR <- FOR %>%
  mutate(
    across(all_of(yeti_vars), ~ ifelse(.x > 5.5, NA_real_, as.numeric(.x)))
  )

#0-3 instead of 1-4
FOR <- FOR %>%
  mutate(
    across(all_of(yeti_vars), ~ ifelse(is.na(.x), NA_real_, .x - 1))
  )


#Calculation
FOR <- FOR %>%
  mutate(
    yetitotal = rowSums(across(all_of(yeti_vars)), na.rm = FALSE),
    yetitot   = yetitotal * 26
  )

FOR <- FOR %>%
  mutate(
    anxF      = rowSums(across(all_of(anx_items)),    na.rm = FALSE),
    yetianx   = anxF * length(anx_items),
    
    depF      = rowSums(across(all_of(dep_items)),    na.rm = FALSE),
    yetidep   = depF * length(dep_items),
    
    afflabF   = rowSums(across(all_of(afflab_items)), na.rm = FALSE),
    yetiafflab= afflabF * length(afflab_items),
    
    PLEF      = rowSums(across(all_of(ple_items)),    na.rm = FALSE),
    yetipsych = PLEF * length(ple_items),
    
    basicF    = rowSums(across(all_of(basic_items)),  na.rm = FALSE),
    yetibs    = basicF * length(basic_items),
    
    sleepF    = rowSums(across(all_of(sleep_items)),  na.rm = FALSE),
    yetisleep = sleepF * length(sleep_items)
  )

