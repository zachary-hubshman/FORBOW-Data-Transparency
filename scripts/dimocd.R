##Generating dimocd
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

#OCI
OCI_Obsessions <- c("oci_sr_1", "oci_sr_12", "oci_sr_13", "oci_sr_17", "oci_sr_20", "oci_sr_28", "oci_sr_30", "oci_sr_33")
OCI_Washing <- c("oci_sr_2", "oci_sr_4", "oci_sr_8","oci_sr_21", "oci_sr_22", "oci_sr_27", "oci_sr_38", "oci_sr_42")
OCI_Check <- c("oci_sr_3", "oci_sr_7", "oci_sr_9", "oci_sr_10", "oci_sr_19", "oci_sr_24", "oci_sr_31", "oci_sr_32", "oci_sr_40")
OCI_Hoard <- c("oci_sr_6", "oci_sr_11", "oci_sr_34")
OCI_Doubt <- c("oci_sr_26", "oci_sr_37", "oci_sr_41")
OCI_Neutral <- c("oci_sr_5", "oci_sr_16", "oci_sr_18", "oci_sr_25", "oci_sr_36", "oci_sr_39")
OCI_Order <- c("oci_sr_14", "oci_sr_15", "oci_sr_23", "oci_sr_29", "oci_sr_35")

FOR <- FOR %>%
  mutate(
    across(any_of(c(OCI_Obsessions, OCI_Washing, OCI_Check, 
                    OCI_Hoard, OCI_Doubt, OCI_Neutral, OCI_Order)),
           ~ {
             x <- readr::parse_number(as.character(.x))
             dplyr::if_else(x > 5, NA_real_, x)
           })
  )

FOR <- FOR %>%
  mutate(
    OCI_Obsess = rowSums(pick(any_of(OCI_Obsessions))),
    OCI_Wash   = rowSums(pick(any_of(OCI_Washing))),
    OCI_Check  = rowSums(pick(any_of(OCI_Check))),
    OCI_Hoard  = rowSums(pick(any_of(OCI_Hoard))),
    OCI_Doubt  = rowSums(pick(any_of(OCI_Doubt))),
    OCI_Neutrl = rowSums(pick(any_of(OCI_Neutral))),
    OCI_Order  = rowSums(pick(any_of(OCI_Order))),
    
    OCI_Total  = rowSums(
      pick(any_of(c(OCI_Obsessions, OCI_Washing, OCI_Check,
                    OCI_Hoard, OCI_Doubt, OCI_Neutral, OCI_Order)))
    )
  )

table(FOR$OCI_Total)

#Generate dimocd
FOR <- FOR %>%
  mutate(
    choci_tot_p_z = as.numeric(scale(choci_tot_p)),
    choci_tot_y_z = as.numeric(scale(choci_tot_y)),
    oci_tot_z     = as.numeric(scale(OCI_Total)),
    
    dimocd = if_else(
      rowSums(!is.na(pick(c(choci_tot_p_z, choci_tot_y_z, oci_tot_z)))) == 0,
      NA_real_,
      rowMeans(pick(c(choci_tot_p_z, choci_tot_y_z, oci_tot_z)), na.rm = TRUE)
    )
  )
table(FOR$dimocd)
sum(!is.na(FOR$dimocd))
