##Generating dimocd


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
