
#Gen dimdep
# 2) Create z-scores and dimensional depression
FOR <- FOR %>%
  mutate(

    MFQ_C_z = as.numeric(scale(mfq_y_tot)),
    MFQ_P_z = as.numeric(scale(mfq_p_tot)),

    dimdep   = rowMeans(across(c(MFQ_C_z, MFQ_P_z)), na.rm = TRUE)
  )
