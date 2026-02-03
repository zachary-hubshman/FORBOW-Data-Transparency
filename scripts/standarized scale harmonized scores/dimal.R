#Creating dimensions of affective labiality
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

