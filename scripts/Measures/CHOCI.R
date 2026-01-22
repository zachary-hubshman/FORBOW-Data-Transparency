##Choci
##https://novopsych.com/assessments/diagnosis/obsessional-compulsive-inventory-ocd-child-self-report/
#Compulsion Symptoms 
choci_y_vars <- paste0("choci_sr_p1_", 1:10)
choci_p_vars <- paste0("choci_pr_p1_", 1:10)

FOR <- FOR %>%
  mutate(
    across(any_of(c(choci_y_vars, choci_p_vars)),
           ~ {
             x <- readr::parse_number(as.character(.x))
             dplyr::if_else(x > 4, NA_real_, x)
           })
  )

FOR <- FOR %>%
  mutate(
    compulsion_symp_y = rowSums(pick(any_of(choci_y_vars))), 
    compulsion_symp_p = rowSums(pick(any_of(choci_p_vars)))
  )
#Compulsion Impairment
choci_y_vars <- paste0("choci_sr_p1_", 11:16)
choci_p_vars <- paste0("choci_pr_p1_", 11:16)

FOR <- FOR %>%
  mutate(
    across(any_of(c(choci_y_vars, choci_p_vars)),
           ~ {
             x <- readr::parse_number(as.character(.x))
             dplyr::if_else(x > 4, NA_real_, x)
           })
  )

FOR <- FOR %>%
  mutate(
    compulsion_impr_y = rowSums(pick(any_of(choci_y_vars))), 
    compulsion_impr_p = rowSums(pick(any_of(choci_p_vars)))
  )

#Obsession Symptoms
choci_y_vars <- paste0("choci_sr_p2_", 1:10)
choci_p_vars <- paste0("choci_pr_p2_", 1:10)

FOR <- FOR %>%
  mutate(
    across(any_of(c(choci_y_vars, choci_p_vars)),
           ~ {
             x <- readr::parse_number(as.character(.x))
             dplyr::if_else(x > 4, NA_real_, x)
           })
  )

FOR <- FOR %>%
  mutate(
    obsession_symp_y = rowSums(pick(any_of(choci_y_vars))), 
    obsession_symp_p = rowSums(pick(any_of(choci_p_vars)))
  )

#Obsession Impairment
choci_y_vars <- paste0("choci_sr_p2_", 11:16)
choci_p_vars <- paste0("choci_pr_p2_", 11:16)

FOR <- FOR %>%
  mutate(
    across(any_of(c(choci_y_vars, choci_p_vars)),
           ~ {
             x <- readr::parse_number(as.character(.x))
             dplyr::if_else(x > 4, NA_real_, x)
           })
  )

FOR <- FOR %>%
  mutate(
    obsession_impr_y = rowSums(pick(any_of(choci_y_vars))), 
    obsession_impr_p = rowSums(pick(any_of(choci_p_vars)))
  )

#Total Symptom
FOR$choci_symp_tot_y <- FOR$compulsion_symp_y + FOR$obsession_symp_y
FOR$choci_symp_tot_p <- FOR$compulsion_symp_p + FOR$obsession_symp_p

#Total Impairment
FOR$choci_impr_tot_y <- FOR$compulsion_impr_y + FOR$obsession_impr_y
FOR$choci_impr_tot_p <- FOR$compulsion_impr_p + FOR$obsession_impr_p

#Total CHOCI
FOR$choci_tot_y <- FOR$choci_symp_tot_y + FOR$choci_impr_tot_y
FOR$choci_tot_p <- FOR$choci_symp_tot_p + FOR$choci_impr_tot_p
