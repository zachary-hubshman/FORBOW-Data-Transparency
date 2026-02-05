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
    IPAQ_Done = if_else(
      if_all(all_of(IPAQ_YoN), ~ is.na(.x) | .x == ""),
      0L,
      1L
    ),
    IPAQ_N_miss = rowSums(
      across(all_of(IPAQ_YoN), ~ is.na(.x) | .x == "")
    )
  )
  
  FOR2 <- FOR %>%
    mutate(
      ipaq_2 = case_when(
        ipaq_2 %in% c(
          "no vigorous job",
          "no",
          "None",
          "There is no question 4?"
        ) ~ "0",
        ipaq_2 == "5 days"  ~ "5",
        ipaq_2 == "2 hours" ~ "0",
        TRUE ~ ipaq_2
      ),
      ipaq_2 = as.numeric(ipaq_2)
    )
  
  FOR2 <-FOR2 %>%
    mutate( 
      ipaq_3_hr = case_when(
        ipaq_3_hr == "1 hour" ~ "1", 
        ipaq_3_hr == "12hr" ~ "12", 
        ipaq_3_hr == "3-4" ~ "3.5",
        ipaq_3_hr == "6-7" ~ "6.5",
        ipaq_3_hr == "less than 1" ~ "0",
        ipaq_3_hr %in% c("n/a", "na", "Nil") ~ "0", 
        TRUE ~ ipaq_3_hr
        ),
        ipaq_3_hr = as.numeric(ipaq_3_hr)
      )
  
  table(FOR$ipaq_3_min)
FOR2 <- FOR2 %>% 
  mutate(
     ipaq_3_min = case_when(
     ipaq_3_min %in% c("n/a", "na", "Nil") ~ "0",
     ipaq_3_min == "120 minute" ~ "120",
     TRUE ~ ipaq_3_min
    ),
    ipaq_3_min = as.numeric(ipaq_3_min)
  )

table(FOR$ipaq_5_hr)

FOR2 <- FOR2 %>%
  mutate(
    ipaq_4 = case_when(
      ipaq_4 %in% c("n/a", "Nil", "no", "no moderate job", "None") ~ "0",
      ipaq_4 == "5 hours" ~ "0",
      ipaq_4 == "4 days" ~ "4",
      TRUE ~ ipaq_4
    ),
    ipaq_4 = as.numeric(ipaq_4)
  )

FOR2 <- FOR2 %>%
  mutate(
    ipaq_5_hr = case_when(
      ipaq_5_hr %in% c("n/a", "na", "Nil", "less than 1") ~ "0",
      ipaq_5_hr == "1-2" ~ "1.5",
      ipaq_5_hr == "5 hours" ~ "5",
      ipaq_5_hr == "1 hour" ~ "1",
      ipaq_5_hr == "9/10" ~ "9.5",
      TRUE ~ ipaq_5_hr
    ),
    ipaq_5_hr = as.numeric(ipaq_5_hr)
  )
table(FOR2$ipaq_6)

FOR2 <- FOR2 %>%
  mutate(ipaq_5_min = case_when(
    ipaq_5_min %in% c("n/a", "na", "Nil") ~ "0",
    TRUE ~ ipaq_5_min
  ),
  ipaq_5_min = as.numeric(ipaq_5_min)
  )


FOR2 <- FOR2 %>%
  mutate(ipaq_6 = case_when(
         ipaq_6 %in% c("no", "no job related walking", "Nilâ\u0081") ~ "0",
         ipaq_6 == "4 days" ~ "4",
         ipaq_6 == "1-5" ~ "3", 
         TRUE ~ ipaq_6
  ),
  ipaq_6 = as.numeric(ipaq_6)
)

table(FOR2$ipaq_7_min)

FOR2 <- FOR2 %>%
  mutate(ipaq_7_hr = case_when(
    ipaq_7_hr %in% c("I stand in one spot most of the day just walking small bursts ", "less than 1", "n/a", "Nil") ~ "0",
    ipaq_7_hr == "1-2" ~ "1.5",
    ipaq_7_hr == "1hr" ~ "1",
    ipaq_7_hr == "6 hours" ~ "6",
    ipaq_7_hr == "8-10" ~ "9",
    ipaq_7_hr == "8-9" ~ "8.5",
    TRUE ~ ipaq_7_hr
  ),
  ipaq_7_hr = as.numeric(ipaq_7_hr))

FOR2 <- FOR2 %>%
  mutate(ipaq_7_min = case_when(
    ipaq_7_min %in% c("n/a", "Nil") ~ "0",
    TRUE ~ ipaq_7_min
  ),
  ipaq_7_min = as.numeric(ipaq_7_min))


