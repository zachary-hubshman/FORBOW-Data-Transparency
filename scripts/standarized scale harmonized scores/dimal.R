#Creating dimensions of affective labiality

##CALS 
cals_youth <- paste0("cals_",1:20)
cals_parent <- paste0("cals_pr_", 1:20)


FOR <- FOR %>%
  mutate(
    across(
      any_of(c(cals_youth, cals_parent)),
      ~ readr::parse_number(as.character(.x))
    )
  )

FOR <- FOR %>%
  # Recode 9s to NA
  mutate(across(all_of(cals_youth), ~na_if(.x, 9))) %>%
  mutate(across(all_of(cals_parent), ~na_if(.x, 9))) %>%
  mutate(
    cals_y_tot = rowSums(pick(any_of(cals_youth))), 
    cals_p_tot = rowSums(pick(any_of(cals_parent)))
  )

##ALS
als <- paste0("als_", 1:54)

FOR <- FOR %>%
  mutate(
    across(
      any_of(als),
      ~ readr::parse_number(as.character(.x))
    )
  ) %>%
  mutate(
    across(
      any_of(als),
      ~ dplyr::case_when(
        .x == 3 ~ 0,
        .x == 2 ~ 1,
        .x == 1 ~ 2,
        .x == 0 ~ 3,
        TRUE    ~ NA_real_
      )
    )
  )

FOR <- FOR %>%
  # Recode 9s to NA
  mutate(across(all_of(als), ~na_if(.x, 9))) %>%
  mutate(
    als_total = rowSums(pick(any_of(als)))
  )


#Dimal
FOR <- FOR %>%
  mutate(
    cals_tot_y_z  = as.numeric(scale(cals_y_tot)),
    cals_tot_p_z  = as.numeric(scale(cals_p_tot)),
    als_tot_z     = as.numeric(scale(als_total)),
    
    dimal = if_else(
      rowSums(!is.na(pick(c(cals_tot_y_z, cals_tot_p_z, als_tot_z)))) == 0,
      NA_real_,
      rowMeans(pick(c(cals_tot_y_z, cals_tot_p_z, als_tot_z)), na.rm = TRUE)
    )
  )

