FOR$iid <- as.numeric(FOR$iid)
#Originally ccCan was cannabis, should this be different?
FOR <- FOR %>%
  mutate(
    ccSUD = ifelse(iid == 443 & time_point %in% c(2, 3), 1, ccSUD),
    ccCan = ifelse((iid == 443 & time_point == 3) |
                     (iid == 321 & time_point %in% c(3, 4, 5)), 1, ccCan),
    ccPsychosis = ifelse((iid == 443 & time_point == 3) |
                           (iid == 281 & time_point == 5), 1, ccPsychosis),
    clSUD = ifelse((iid == 321 & time_point == 5) |
                     (iid == 443 & time_point %in% c(2, 3)), 1, clSUD),
    ccBorderline = ifelse(iid == 321 & time_point == 5, 1, ccBorderline),
    clBorderline = ifelse(iid == 321 & time_point == 5, 1, clBorderline),
    ccAntisocial = ifelse(iid == 321 & time_point == 5, 1, ccAntisocial),
    clAntisocial = ifelse(iid == 321 & time_point == 5, 1, clAntisocial),
    clPsychosis = ifelse((iid == 443 & time_point == 3) |
                           (iid == 281 & time_point > 4.5), 1, clPsychosis),
    oSCZ = ifelse((iid == 443 & time_point == 3) |
                    (iid == 281 & time_point == 5), 1, oSCZ)
  )