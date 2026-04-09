#Diagnosis
###Current Diagnosis
### List of FORBOW Diagnoses


KSADS_ALL <- c('MDD', 'DEP', 'Dysthy', 'BP', 'BPI', 'BPII', 'MOOD',
               'Psychosis', 'iPsychosis', 'SCZ', 'SchAff',
               'SUD', 'Alc', 'Can', 'Dependence',
               'Anx', 'Panic', 'OthAnx', 'GAD', 'SocialP', 'PTSD', 'Agora', 'SpecificP', 'OCD', 'SAD',
               'ADHD', 'ODD', 'Conduct', 'Disrupt',
               'ED', 'EDNOS', 'Anor', 'Bulims',
               'Autism', 'ASD', 'Tic', 'Touret',
               'LD', 'LDs', 'LDg',
               'DMDD', 'Sleep',
               'Borderline', 'Antisocial', 'PersonalityDisord')


##Clinical Diagnosis from Consesus
Diagnosis_vars <- paste0("cd_c_dx_", 1:10)

#Depression
FOR$ccMDD <- 0
FOR$ccDEP <- 0 
FOR$ccDysthy <- 0
for (i in 1:10) {
  dx_col <- paste0("cd_c_dx_", i)
  conf_col <- paste0("cd_c_confirmed_", i)
  text_col <- paste0("cd_c_dx_other_", i)
  rows_to_update <- FOR[[dx_col]] == 1 & FOR[[conf_col]] != 0
  FOR$ccMDD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(1,2,3) & FOR[[conf_col]] != 0
  FOR$ccDEP[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("depress", "Depress", "Major Depress", "major depress") & FOR[[conf_col]] != 0
  FOR$ccDEP[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(2) & FOR[[conf_col]] != 0
  FOR$ccDysthy[rows_to_update] <- 1
}

#Mania
FOR$ccBP <- 0
FOR$ccBPI <- 0
FOR$ccBPII <- 0
for (i in 1:10) {
  dx_col <- paste0("cd_c_dx_", i)
  conf_col <- paste0("cd_c_confirmed_", i)
  text_col <- paste0("cd_c_dx_other_", i)
  rows_to_update <- FOR[[dx_col]] %in% c(4, 5,6,7) & FOR[[conf_col]] != 0
  FOR$ccBP[rows_to_update] <- 1
  rows_to_update <- FOR[[dx_col]] %in% c(4) & FOR[[conf_col]] != 0
  FOR$ccBPI[rows_to_update] <- 1
  rows_to_update <- FOR[[dx_col]] %in% c(5) & FOR[[conf_col]] != 0
  FOR$ccBPI[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("Mania", "mania", "Bipolar", "bipolar") & FOR[[conf_col]] != 0
  FOR$ccBP[rows_to_update] <- 1
}
table(FOR$ccBP)
table(FOR$ccBPI)
table(FOR$ccBPII)


##Psychosis
FOR$ccPsychosis <- 0
FOR$cciPsychosis <- 0
FOR$ccSCZ <- 0
FOR$ccSchAff <- 0
for (i in 1:10) {
  dx_col <- paste0("cd_c_dx_", i)
  conf_col <- paste0("cd_c_confirmed_", i)
  rows_to_update <- FOR[[dx_col]] %in% c(8,9,10,11,12,13,14) & FOR[[conf_col]] != 0 
  FOR$ccPsychosis[rows_to_update] <- 1
  rows_to_update <- FOR[[dx_col]] %in% c(8) & FOR[[conf_col]] != 0 
  FOR$ccSCZ[rows_to_update] <- 1
  rows_to_update <- FOR[[dx_col]] %in% c(9) & FOR[[conf_col]] != 0 
  FOR$ccSchAff[rows_to_update] <- 1
}

#Personality
FOR$ccBorderline <- 0
FOR$ccAntisocial <- 0
FOR$ccPersonalityDisord <- 0
for (i in 1:10) {
  dx_col <- paste0("cd_c_dx_", i)
  conf_col <- paste0("cd_c_confirmed_", i)
  text_col <- paste0("cd_c_dx_other_", i)
  rows_to_update <- FOR[[dx_col]] %in% c(25) & FOR[[conf_col]] != 0 
  FOR$ccBorderline[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("orderline") & FOR[[conf_col]] != 0
  FOR$ccBorderline[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(26) & FOR[[conf_col]] != 0 
  FOR$ccAntisocial[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("ntisocial") & FOR[[conf_col]] != 0
  FOR$ccAntisocial[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(70,71,72,73,74,75,76,77,78,79,80) & FOR[[conf_col]] != 0
  FOR$ccPersonalityDisord[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("personality", "Personality", "antisocial", "Antisocial", "orderline") &FOR[[conf_col]] != 0
  FOR$ccPersonalityDisord[rows_to_update] <- 1
}

#Substance Use
FOR$ccAlc <- 0
FOR$ccCan <- 0
FOR$ccSUD <- 0
FOR$ccDependence <- 0
for (i in 1:10) {
  dx_col <- paste0("cd_c_dx_", i)
  conf_col <- paste0("cd_c_confirmed_", i)
  rows_to_update <- FOR[[dx_col]] %in% c(38,39) & FOR[[conf_col]] != 0 
  FOR$ccAlc[rows_to_update] <- 1
  rows_to_update <- FOR[[dx_col]] %in% c(40,41) & FOR[[conf_col]] != 0 
  FOR$ccCan[rows_to_update] <- 1
  rows_to_update <- FOR[[dx_col]] %in% c(38,39,40,41,42,43) & FOR[[conf_col]] != 0 
  FOR$ccSUD[rows_to_update] <- 1
  rows_to_update <- FOR[[dx_col]] %in% c(39,41,43) & FOR[[conf_col]] != 0 
  FOR$ccDependence[rows_to_update] <- 1
}

#Anxiety Disorders
FOR$ccAnx <- 0
FOR$ccSAD <- 0  #Why is SAD 15 and why is anxiety within SAD?
FOR$ccSpecificP <- 0
FOR$ccSocialP <- 0 
FOR$ccAgora <- 0
FOR$ccPanic <- 0
FOR$ccGAD <- 0
FOR$ccOCD <- 0
FOR$ccPTSD <- 0
FOR$ccOthAnx <- 0
for (i in 1:10) {
  dx_col <- paste0("cd_c_dx_", i)
  conf_col <- paste0("cd_c_confirmed_", i)
  text_col <- paste0("cd_c_dx_other_", i)
  
  rows_to_update <- FOR[[dx_col]] %in% c(15,16,17,18,19,20,21,22,24) & FOR[[conf_col]] != 0    #Anxiety Disorders
  FOR$ccAnx[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("nxiety", "GAD", "Mutism", "mutism") & FOR[[conf_col]] != 0    #Anxiety disorders
  FOR$ccAnx[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(15) & FOR[[conf_col]] != 0    #SAD?
  FOR$ccSAD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(16) & FOR[[conf_col]] != 0   #Specific P
  FOR$ccSpecificP[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(17) & FOR[[conf_col]] != 0   #Social P
  FOR$ccSocialP[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("Social") & FOR[[conf_col]] != 0 #Social P
  FOR$ccSocialP[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(18) & FOR[[conf_col]] != 0        #AGORA
  FOR$ccAgora[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(19) & FOR[[conf_col]] != 0       #Panic
  FOR$ccPanic[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(20) & FOR[[conf_col]] != 0       #GAD
  FOR$ccGAD[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("GAD") & FOR[[conf_col]] != 0 #GAD
  FOR$ccGAD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(21) & FOR[[conf_col]] != 0       #OCD
  FOR$ccOCD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(22) & FOR[[conf_col]] != 0       #PTSD
  FOR$ccPTSD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(24) & FOR[[conf_col]] != 0       #OthAnx
  FOR$ccOthAnx[rows_to_update] <- 1
}

#Eating Disorder
FOR$ccAnor <- 0
FOR$ccBulims <- 0
FOR$ccEDNOS <- 0
FOR$ccED <- 0
for (i in 1:10) {
  dx_col <- paste0("cd_c_dx_", i)
  conf_col <- paste0("cd_c_confirmed_", i)
  text_col <- paste0("cd_c_dx_other_", i)
  
  rows_to_update <- FOR[[dx_col]] %in% c(28) & FOR[[conf_col]] != 0 
  FOR$ccAnor[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(29) & FOR[[conf_col]] != 0 
  FOR$ccBulims[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(30) & FOR[[conf_col]] != 0 
  FOR$ccEDNOS[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(28,29,30) & FOR[[conf_col]] != 0 
  FOR$ccED[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("Eating","eating","Pica", "pica") & FOR[[conf_col]] != 0 
  FOR$ccED[rows_to_update] <- 1
}

##Development
FOR$ccADHD <- 0
FOR$ccODD <- 0
FOR$ccConduct <- 0
FOR$ccDisrupt <- 0
for (i in 1:10) {
  dx_col <- paste0("cd_c_dx_", i)
  conf_col <- paste0("cd_c_confirmed_", i)
  text_col <- paste0("cd_c_dx_other_", i)
  
  rows_to_update <- FOR[[dx_col]] %in% c(31,32,33) & FOR[[conf_col]] != 0    #ADHD
  FOR$ccADHD[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("ADHD") & FOR[[conf_col]] != 0 
  FOR$ccADHD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(34) & FOR[[conf_col]] != 0         #ODD
  FOR$ccODD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(35) & FOR[[conf_col]] != 0        #Conduct
  FOR$ccConduct[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(34,35) & FOR[[conf_col]] != 0        #Disruptive Disorder
  FOR$ccDisrupt[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("isruptive") & FOR[[conf_col]] != 0 
  FOR$ccDisrupt[rows_to_update] <- 1
}

#Neuro
FOR$ccTic <- 0
FOR$ccTouret <- 0
FOR$ccAutism <- 0
FOR$ccASD <- 0
for (i in 1:10) {
  dx_col <- paste0("cd_c_dx_", i)
  conf_col <- paste0("cd_c_confirmed_", i)
  text_col <- paste0("cd_c_dx_other_", i)
  
  rows_to_update <- FOR[[dx_col]] %in% c(36,37) & FOR[[conf_col]] != 0    #TIC
  FOR$ccTic[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("Tic", "tic", "ouret") & FOR[[conf_col]] != 0 
  FOR$ccTic[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(36) & FOR[[conf_col]] != 0         #Touret
  FOR$ccTouret[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("ouret") & FOR[[conf_col]] != 0 
  FOR$ccTouret[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(44) & FOR[[conf_col]] != 0        #Autism
  FOR$ccAutism[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(44,45, 46) & FOR[[conf_col]] != 0        #Autism Spectrum
  FOR$ccASD[rows_to_update] <- 1
}

#Learning Disabilities
FOR$ccLD <- 0
FOR$ccLDs <- 0
FOR$ccLDg <- 0
for (i in 1:10) {
  dx_col <- paste0("cd_c_dx_", i)
  conf_col <- paste0("cd_c_confirmed_", i)
  text_col <- paste0("cd_c_dx_other_", i)
  
  rows_to_update <- FOR[[text_col]] %in% c("LD", "Learning", "learning", "Intellectual", "intellectual", "Fragile", "Dyslexia", "dyslexia", "Dysgraphia", "dysgraphia", "Retardation", "retardation", "Communication", "disability")
  FOR$ccLD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[text_col]] %in% c("Spelling", "spelling", "Reading", "reading", "Non-Verbal", "Processing Speed", "Phonological", "Dyslexia", "dyslexia", "dysgraphia", "Dysgraphia", "Communication") 
  FOR$ccLDs[rows_to_update] <- 1
  
  rows_to_update <- FOR[[text_col]] %in% c("Intellectual", "intellectual", "Retardation", "retardation") 
  FOR$ccLDg[rows_to_update] <- 1
}

#Sleep
FOR$ccSleep <- 0
for (i in 1:10) {
  dx_col <- paste0("cd_c_dx_", i)
  conf_col <- paste0("cd_c_confirmed_", i)
  text_col <- paste0("cd_c_dx_other_", i)
  rows_to_update <- FOR[[text_col]] %in% c("insomnia", "sleep", "somnia")
  FOR$ccSleep[rows_to_update] <- 1
}


#Disruptive Mood
FOR$ccDMDD <- 0
for (i in 1:10) {
  dx_col <- paste0("cd_c_dx_", i)
  conf_col <- paste0("cd_c_confirmed_", i)
  text_col <- paste0("cd_c_dx_other_", i)
  rows_to_update <- FOR[[text_col]] %in% c("DMDD")
  FOR$ccDMDD[rows_to_update] <- 1
}

FOR <- FOR %>%
  mutate(ccMOOD = ifelse(ccBP == 1 | ccDEP == 1 | ccMDD == 1, 1, 0))


#NSSI
FOR$ccNSSI <- 0
for (i in 1:10) {
  dx_col <- paste0("cd_c_dx_", i)
  conf_col <- paste0("cd_c_confirmed_", i)
  text_col <- paste0("cd_c_dx_other_", i)
  
  rows_to_update <- FOR[[dx_col]] %in% c(84) & FOR[[conf_col]] != 0 
  FOR$ccNSSI[rows_to_update] <- 1
}
