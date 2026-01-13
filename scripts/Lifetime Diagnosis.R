#Create lifetime based on confirmed lifetime.
##Clinical Diagnosis from Consesus

#Depression
##This needs double checking on the text column
# FOR$clMDD 
# FOR$clDEP  
# FOR$clDysthy 
for (i in 1:10) {
  dx_col <- paste0("cd_lt_dx_", i)
  conf_col <- paste0("cd_lt_confirmed_", i)
  text_col <- paste0("cd_lt_dx_other_", i)
  rows_to_update <- FOR[[dx_col]] == 1 & FOR[[conf_col]] != 0
  FOR$clMDD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(1,2,3) & FOR[[conf_col]] != 0
  FOR$clDEP[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("depress", "Depress", "Major Depress", "major depress") & FOR[[conf_col]] != 0
  FOR$clDEP[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(2) & FOR[[conf_col]] != 0
  FOR$clDysthy[rows_to_update] <- 1
}


##Mania
# FOR$clBP 
# FOR$clBPI 
# FOR$clBPII 
for (i in 1:10) {
  dx_col <- paste0("cd_lt_dx_", i)
  conf_col <- paste0("cd_lt_confirmed_", i)
  text_col <- paste0("cd_lt_dx_other_", i)
  rows_to_update <- FOR[[dx_col]] %in% c(4, 5,6,7) & FOR[[conf_col]] != 0
  FOR$clBP[rows_to_update] <- 1
  rows_to_update <- FOR[[dx_col]] %in% c(4) & FOR[[conf_col]] != 0
  FOR$clBPI[rows_to_update] <- 1
  rows_to_update <- FOR[[dx_col]] %in% c(5) & FOR[[conf_col]] != 0
  FOR$clBPI[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("Mania", "mania", "Bipolar", "bipolar") & FOR[[conf_col]] != 0
  FOR$clBP[rows_to_update] <- 1
}


##Psychosis
# FOR$clPsychosis 
# FOR$cliPsychosis 
# FOR$clSCZ 
# FOR$clSchAff 
for (i in 1:10) {
  dx_col <- paste0("cd_lt_dx_", i)
  conf_col <- paste0("cd_lt_confirmed_", i)
  rows_to_update <- FOR[[dx_col]] %in% c(8,9,10,11,12,13,14) & FOR[[conf_col]] != 0 
  FOR$clPsychosis[rows_to_update] <- 1
  rows_to_update <- FOR[[dx_col]] %in% c(8) & FOR[[conf_col]] != 0 
  FOR$clSCZ[rows_to_update] <- 1
  rows_to_update <- FOR[[dx_col]] %in% c(9) & FOR[[conf_col]] != 0 
  FOR$clSchAff[rows_to_update] <- 1
}

##Personality
# FOR$clBorderline 
# FOR$clAntisocial 
# FOR$clPersonalityDisord 
for (i in 1:10) {
  dx_col <- paste0("cd_lt_dx_", i)
  conf_col <- paste0("cd_lt_confirmed_", i)
  text_col <- paste0("cd_lt_dx_other_", i)
  rows_to_update <- FOR[[dx_col]] %in% c(25) & FOR[[conf_col]] != 0 
  FOR$clBorderline[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("orderline") & FOR[[conf_col]] != 0
  FOR$clBorderline[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(26) & FOR[[conf_col]] != 0 
  FOR$clAntisocial[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("ntisocial") & FOR[[conf_col]] != 0
  FOR$clAntisocial[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(70,71,72,73,74,75,76,77,78,79,80) & FOR[[conf_col]] != 0
  FOR$clPersonalityDisord[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("personality", "Personality", "antisocial", "Antisocial", "orderline") &FOR[[conf_col]] != 0
  FOR$clPersonalityDisord[rows_to_update] <- 1
}

##Substance Use
# FOR$clAlc 
# FOR$clCan 
# FOR$clSUD 
# FOR$clDependence
for (i in 1:10) {
  dx_col <- paste0("cd_lt_dx_", i)
  conf_col <- paste0("cd_lt_confirmed_", i)
  rows_to_update <- FOR[[dx_col]] %in% c(38,39) & FOR[[conf_col]] != 0 
  FOR$clAlc[rows_to_update] <- 1
  rows_to_update <- FOR[[dx_col]] %in% c(40,41) & FOR[[conf_col]] != 0 
  FOR$clCan[rows_to_update] <- 1
  rows_to_update <- FOR[[dx_col]] %in% c(38,39,40,41,42,43) & FOR[[conf_col]] != 0 
  FOR$clSUD[rows_to_update] <- 1
  rows_to_update <- FOR[[dx_col]] %in% c(39,41,43) & FOR[[conf_col]] != 0 
  FOR$clDependence[rows_to_update] <- 1
}

##Anxiety Disorders
# FOR$clAnx
# FOR$clSAD
# FOR$clSpecificP 
# FOR$clSocialP 
# FOR$clAgora 
# FOR$clPanic 
# FOR$clGAD 
# FOR$clOCD 
# FOR$clPTSD 
# FOR$clOthAnx 
for (i in 1:10) {
  dx_col <- paste0("cd_lt_dx_", i)
  conf_col <- paste0("cd_lt_confirmed_", i)
  text_col <- paste0("cd_lt_dx_other_", i)
  
  rows_to_update <- FOR[[dx_col]] %in% c(15,16,17,18,19,20,21,22,24) & FOR[[conf_col]] != 0    #Anxiety Disorders
  FOR$clAnx[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("nxiety", "GAD", "Mutism", "mutism") & FOR[[conf_col]] != 0    #Anxiety disorders
  FOR$clAnx[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(15) & FOR[[conf_col]] != 0    #SAD?
  FOR$clSAD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(16) & FOR[[conf_col]] != 0   #Specific P
  FOR$clSpecificP[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(17) & FOR[[conf_col]] != 0   #Social P
  FOR$clSocialP[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("Social") & FOR[[conf_col]] != 0 #Social P
  FOR$clSocialP[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(18) & FOR[[conf_col]] != 0        #AGORA
  FOR$clAgora[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(19) & FOR[[conf_col]] != 0       #Panic
  FOR$clPanic[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(20) & FOR[[conf_col]] != 0       #GAD
  FOR$clGAD[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("GAD") & FOR[[conf_col]] != 0 #GAD
  FOR$clGAD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(21) & FOR[[conf_col]] != 0       #OCD
  FOR$clOCD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(22) & FOR[[conf_col]] != 0       #PTSD
  FOR$clPTSD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(24) & FOR[[conf_col]] != 0       #OthAnx
  FOR$clOthAnx[rows_to_update] <- 1
}

#Eating Disorder
# FOR$clAnor
# FOR$clBulims 
# FOR$clEDNOS 
# FOR$clED 
for (i in 1:10) {
  dx_col <- paste0("cd_lt_dx_", i)
  conf_col <- paste0("cd_lt_confirmed_", i)
  text_col <- paste0("cd_lt_dx_other_", i)
  
  rows_to_update <- FOR[[dx_col]] %in% c(28) & FOR[[conf_col]] != 0 
  FOR$clAnor[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(29) & FOR[[conf_col]] != 0 
  FOR$clBulims[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(30) & FOR[[conf_col]] != 0 
  FOR$clEDNOS[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(28,29,30) & FOR[[conf_col]] != 0 
  FOR$clED[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("Eating","eating","Pica", "pica") & FOR[[conf_col]] != 0 
  FOR$clED[rows_to_update] <- 1
}

##Development
# FOR$clADHD 
# FOR$clODD 
# FOR$clConduct 
# FOR$clDisrupt 
for (i in 1:10) {
  dx_col <- paste0("cd_lt_dx_", i)
  conf_col <- paste0("cd_lt_confirmed_", i)
  text_col <- paste0("cd_lt_dx_other_", i)
  
  rows_to_update <- FOR[[dx_col]] %in% c(31,32,33) & FOR[[conf_col]] != 0    #ADHD
  FOR$clADHD[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("ADHD") & FOR[[conf_col]] != 0 
  FOR$clADHD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(34) & FOR[[conf_col]] != 0         #ODD
  FOR$clODD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(35) & FOR[[conf_col]] != 0        #Conduct
  FOR$clConduct[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(34,35) & FOR[[conf_col]] != 0        #Disruptive Disorder
  FOR$clDisrupt[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("isruptive") & FOR[[conf_col]] != 0 
  FOR$clDisrupt[rows_to_update] <- 1
}

#Neuro
# FOR$ccTic 
# FOR$ccTouret 
# FOR$ccAutism 
# FOR$ccASD 
for (i in 1:10) {
  dx_col <- paste0("cd_lt_dx_", i)
  conf_col <- paste0("cd_lt_confirmed_", i)
  text_col <- paste0("cd_lt_dx_other_", i)
  
  rows_to_update <- FOR[[dx_col]] %in% c(36,37) & FOR[[conf_col]] != 0    #TIC
  FOR$clTic[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("Tic", "tic", "ouret") & FOR[[conf_col]] != 0 
  FOR$clTic[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(36) & FOR[[conf_col]] != 0         #Touret
  FOR$clTouret[rows_to_update] <- 1
  rows_to_update <- FOR[[text_col]] %in% c("ouret") & FOR[[conf_col]] != 0 
  FOR$clTouret[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(44) & FOR[[conf_col]] != 0        #Autism
  FOR$clAutism[rows_to_update] <- 1
  
  rows_to_update <- FOR[[dx_col]] %in% c(44,45, 46) & FOR[[conf_col]] != 0        #Autism Spectrum
  FOR$clASD[rows_to_update] <- 1
}

#Learning Disabilities
# FOR$clLD 
# FOR$clLDs 
# FOR$clLDg 
for (i in 1:10) {
  dx_col <- paste0("cd_lt_dx_", i)
  conf_col <- paste0("cd_lt_confirmed_", i)
  text_col <- paste0("cd_lt_dx_other_", i)
  
  rows_to_update <- FOR[[text_col]] %in% c("LD", "Learning", "learning", "Intellectual", "intellectual", "Fragile", "Dyslexia", "dyslexia", "Dysgraphia", "dysgraphia", "Retardation", "retardation", "Communication", "disability")
  FOR$clLD[rows_to_update] <- 1
  
  rows_to_update <- FOR[[text_col]] %in% c("Spelling", "spelling", "Reading", "reading", "Non-Verbal", "Processing Speed", "Phonological", "Dyslexia", "dyslexia", "dysgraphia", "Dysgraphia", "Communication") 
  FOR$clLDs[rows_to_update] <- 1
  
  rows_to_update <- FOR[[text_col]] %in% c("Intellectual", "intellectual", "Retardation", "retardation") 
  FOR$clLDg[rows_to_update] <- 1
}

#Sleep
# FOR$clSleep 
for (i in 1:10) {
  dx_col <- paste0("cd_lt_dx_", i)
  conf_col <- paste0("cd_lt_confirmed_", i)
  text_col <- paste0("cd_lt_dx_other_", i)
  rows_to_update <- FOR[[text_col]] %in% c("insomnia", "sleep", "somnia")
  FOR$clSleep[rows_to_update] <- 1
}


#Disruptive Mood
# FOR$clDMDD 
for (i in 1:10) {
  dx_col <- paste0("cd_lt_dx_", i)
  conf_col <- paste0("cd_lt_confirmed_", i)
  text_col <- paste0("cd_lt_dx_other_", i)
  rows_to_update <- FOR[[text_col]] %in% c("DMDD")
  FOR$clDMDD[rows_to_update] <- 1
}

FOR <- FOR %>%
  mutate(clMOOD = ifelse(clBP == 1 | clDEP == 1 | clMDD == 1, 1, 0))



#Create lifetime based on confiremd concrrent
KSADS_ALL <- c('MDD','DEP','Dysthy','BP','BPI','BPII','MOOD',
               'Psychosis','iPsychosis','SCZ','SchAff',
               'SUD','Alc','Can','Dependence',
               'Anx','Panic','OthAnx','GAD','SocialP','PTSD','Agora','SpecificP','OCD','SAD',
               'ADHD','ODD','Conduct','Disrupt',
               'ED','EDNOS','Anor','Bulims',
               'Autism','ASD','Tic','Touret',
               'LD','LDs','LDg',
               'DMDD','Sleep',
               'Borderline','Antisocial','PersonalityDisord')

pb <- txtProgressBar(min = 0, max = length(KSADS_ALL), style = 3)

for (k in seq_along(KSADS_ALL)) {
  
  dx <- KSADS_ALL[k]
  cc_var <- paste0("cc", dx)
  cl_var <- paste0("cl", dx)
  
  FOR <- FOR %>%
    group_by(subject_id) %>%
    arrange(assessment_date, .by_group = TRUE) %>%
    mutate(!!cl_var := as.integer(cumsum(.data[[cc_var]] == 1) > 0)) %>%
    ungroup()
  
  setTxtProgressBar(pb, k)
}

close(pb)
