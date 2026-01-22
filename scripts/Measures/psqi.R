#duration
FOR$psqi_4 <- as.numeric(FOR$psqi_4)
FOR <- FOR %>%
  mutate(
    psqidurat = case_when(
      psqi_4 > 23.99                 ~ NA_real_,
      psqi_4 > 6.99 & psqi_4 < 23.99 ~ 0,
      psqi_4 < 5                    ~ 3,
      psqi_4 < 6                    ~ 2,
      psqi_4 < 7                    ~ 1,
      TRUE                          ~ NA_real_
    )
  )

#Sleep disturbances
dist_vars <- c("psqi_5b","psqi_5c","psqi_5d","psqi_5e",
               "psqi_5f","psqi_5g","psqi_5h","psqi_5i","psqi_5j")

FOR <- FOR %>%
  mutate(
    across(all_of(dist_vars),
           ~ suppressWarnings(as.numeric(as.character(.x))))
  ) %>%
  mutate(
    psqdistscore = rowSums(pick(any_of(dist_vars)), na.rm = TRUE),
    psqidistb = case_when(
      psqdistscore == 0  ~ 0,
      psqdistscore < 9  ~ 1,
      psqdistscore < 18 ~ 2,
      psqdistscore < 99 ~ 3,
      TRUE              ~ NA_real_
    )
  )

#Latency
FOR <- FOR %>%
  mutate(
    across(all_of(c("psqi_2", "psqi_5a")),
           ~ suppressWarnings(as.numeric(as.character(.x))))
  ) %>%
  mutate(
    q2new = case_when(
      psqi_2 > 9999            ~ NA_real_,
      psqi_2 < 15.01           ~ 0,
      psqi_2 < 30.01           ~ 1,
      psqi_2 < 60              ~ 2,
      psqi_2 < 9999            ~ 3,
      TRUE                     ~ NA_real_
    ),
    slatv = rowSums(cbind(psqi_5a, q2new), na.rm = TRUE),
    psqilaten = case_when(
      slatv < 1  ~ 0,
      slatv < 3  ~ 1,
      slatv < 5  ~ 2,
      slatv < 7  ~ 3,
      TRUE       ~ NA_real_
    )
  )

#Daytime Sleepiness

FOR <- FOR %>%
  mutate(
    across(all_of(c("psqi_8", "psqi_9")),
           ~ suppressWarnings(as.numeric(as.character(.x))))
  ) %>%
  mutate(
    sdayv = ifelse(is.na(psqi_8) & is.na(psqi_9), NA_real_,
                   rowSums(cbind(psqi_8, psqi_9), na.rm = TRUE)),
    psqidaydys = case_when(
      sdayv < 1 ~ 0,
      sdayv < 3 ~ 1,
      sdayv < 5 ~ 2,
      sdayv < 7 ~ 3,
      TRUE      ~ NA_real_
    )
  )

#Sleep Efficiency
FOR <- FOR %>%
  mutate(
    psqi_1 = ifelse(str_detect(psqi_1, ":"), str_replace(psqi_1, ":", ""), psqi_1),
    psqi_3 = ifelse(str_detect(psqi_3, ":"), str_replace(psqi_3, ":", ""), psqi_3)
  ) %>%
  mutate(
    across(all_of(c("psqi_1","psqi_3","psqi_4")),
           ~ suppressWarnings(as.numeric(as.character(.x))))
  )

#A
FOR <- FOR %>%
  mutate(
    q1_min = ifelse(is.na(psqi_1), NA_real_,
                    (psqi_1 %/% 100) * 60 + (psqi_1 %% 100)),
    q3_min = ifelse(is.na(psqi_3), NA_real_,
                    (psqi_3 %/% 100) * 60 + (psqi_3 %% 100)),
    
    # signed difference (NOT abs)
    Diffmin_signed = ifelse(is.na(q1_min) | is.na(q3_min),
                            NA_real_,
                            q3_min - q1_min),
    
    # if negative, add 24h in minutes
    Diffmin = ifelse(is.na(Diffmin_signed),
                     NA_real_,
                     ifelse(Diffmin_signed < 0, Diffmin_signed + 1440, Diffmin_signed)),
    
    Diffhour = Diffmin / 60,
    newtib   = Diffhour,
    
    # assuming psqi_4 is HOURS slept
    tmphse = ifelse(is.na(psqi_4) | is.na(newtib) | newtib == 0,
                    NA_real_,
                    (psqi_4 / newtib) * 100)
  )
FOR <- FOR %>%
  mutate(
    psqihse = case_when(
      is.na(tmphse)          ~ NA_real_,
      tmphse > 85            ~ 0,
      tmphse <= 85 & tmphse > 75 ~ 1,
      tmphse <= 75 & tmphse > 65 ~ 2,
      tmphse <= 65           ~ 3
    )
  )



#Sleep Quality and Medicine
FOR <- FOR %>%
  mutate(
    psqislpqual = case_when(
      is.na(psqi_6) ~ NA_real_,
      TRUE ~ as.numeric(psqi_6)
    ),
    psqimeds = case_when(
      is.na(psqi_7) ~ NA_real_,
      TRUE ~ as.numeric(psqi_7)
    )
  )


FOR$psqitotal <- rowSums(
  FOR[, c("psqidurat",
          "psqidistb",
          "psqilaten",
          "psqidaydys",
          "psqihse",
          "psqislpqual",
          "psqimeds")]
)
