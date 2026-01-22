KSADS <- c('MDD', 'BP', 'BPI', 'BPII', 'MOOD', 'Psychosis', 'SCZ', 'SchAff', 'iPsychosis', 'SUD', 'Alc', 'Can', 'Dependence', 'Anx', 'Panic', 'OthAnx', 'GAD', 'SocialP', 'PTSD', 'Agora', 'OCD', 'SAD', 'ADHD', 'ODD', 'Conduct', 'Disrupt', 'ED', 'EDNOS', 'Anor', 'Bulims', 'Autism', 'ASD', 'Tic', 'Touret', 'LD', 'LDs', 'LDg', 'DMDD', 'Sleep', 'Borderline', 'Antisocial', 'PersonalityDisord')

pb <- txtProgressBar(min = 0, max = length(KSADS), style = 3)

for (k in seq_along(KSADS)) {
  
  dx <- KSADS[k]
  cc_var <- paste0("cc", dx)
  o_var  <- paste0("o", dx)
  
  FOR <- FOR %>%
    group_by(subject_id) %>%
    arrange(assessment_date, .by_group = TRUE) %>%
    mutate(
      !!o_var := as.integer(
        row_number() > 1 &
          row_number() == which(.data[[cc_var]] == 1)[1] &
          .data[[cc_var]] == 1
      )
    ) %>%
    ungroup()
  
  setTxtProgressBar(pb, k)
}

close(pb)

