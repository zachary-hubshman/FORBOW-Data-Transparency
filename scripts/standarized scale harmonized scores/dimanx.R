#Scared Z Scores
FOR <- FOR %>%
  
  # Standardize total scores
  mutate(
    Scared_C_Total_z = as.numeric(scale(Scared_C_Total)),
    Scared_P_Total_z = as.numeric(scale(Scared_P_Total))
  )

#SCAS Z Scores
FOR <- FOR %>%
  # Standardize total scores
  mutate(
    Scas_C_z = as.numeric(scale(Scas_C_Total)),
    Scas_P_z = as.numeric(scale(Scas_P_Total))
  )

FOR <- FOR %>%
  # SCAS dimanx
  mutate(dimanx = rowMeans(across(c(Scas_C_z, Scas_P_z, Scared_C_Total_z, Scared_P_Total_z)), na.rm = TRUE)) 

