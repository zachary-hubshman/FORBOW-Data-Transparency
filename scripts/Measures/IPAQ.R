##IPAQ
#https://www.who.int/docs/default-source/ncds/ncd-surveillance/gpaq-analysis-guide.pdf

#Cleaning
IPAQ_YoN  <- c("ipaq_1", "ipaq_2", "ipaq_3_hr", "ipaq_3_min", "ipaq_4", "ipaq_5_hr", "ipaq_5_min", "ipaq_6", 
               "ipaq_7_hr", "ipaq_7_min", "ipaq_8", "ipaq_9_hr", "ipaq_9_min", "ipaq_10", "ipaq_11_hr", 
               "ipaq_11_min", "ipaq_12", "ipaq_13_hr", "ipaq_13_min", "ipaq_14", "ipaq_15_hr", "ipaq_15_min", 
               "ipaq_16", "ipaq_17_hr", "ipaq_17_min", "ipaq_18", "ipaq_19_hr", "ipaq_19_min", "ipaq_20", 
               "ipaq_21_hr", "ipaq_21_min", "ipaq_22", "ipaq_23_hr", "ipaq_23_min", "ipaq_24", "ipaq_25_hr", 
               "ipaq_25_min", "ipaq_26_hr", "ipaq_26_min", "ipaq_27_hr")

IPAQ_Days <- c("ipaq_2", "ipaq_4", "ipaq_6", "ipaq_8", "ipaq_10", "ipaq_12", 
               "ipaq_14", "ipaq_16", "ipaq_18", "ipaq_20", "ipaq_22", "ipaq_24")

FOR <- FOR %>%
  mutate(
    IPAQ_Done   = if_else(if_all(all_of(IPAQ_YoN), is.na), 0L, 1L),
    IPAQ_N_miss = rowSums(is.na(across(all_of(IPAQ_YoN))))
  )

FOR <- FOR %>%
  mutate(
    IPAQ_missing_var = if_else(
      IPAQ_N_miss == 1,
      apply(
        select(., all_of(IPAQ_YoN)),
        1,
        function(x) names(x)[is.na(x)]
      ),
      NA_character_
    )
  )
